/*
	04/05/2017 maksim

	segment data type to match as closely (as reasonable) to the postgres implementation in the schema.sql

	things that are strange: (,) indicates unbounded on both ends (infinite range)
							 (,x] indicates one-sided unbounded
							 "empty" indicates empty
							 the type can also be null
							 [x,x] is a singleton (point in time?)

							 segments are compared by comparing bounds and lower bound and upper bound
							 but two segment endpoints can be LE/GE even if ( [ don't match (i.e. one is
							 inclusive and the other exclusive). two endpoints are equal if they're suprema/infima
							 are the same.

							 milliseconds are optional in the db and get truncated in code should always be used even if
							 .000

	non singleton segments should only be created with right endpoint ) for consistency with data
	already in the database.

    in go seconds are counted relative to January 1, year 1 00:00:00 UTC
    while in postgres they're counted relative to the start of the epoch (unix epoch) i.e 0
    so 00:00:00 is parsed as -31622400 seconds or 31622400000000000 nanoseconds (1 year before january 1 01.
    EPOCHSHIFT shifts 00:00:00 forward by simply addding 31622400000000000 to time.Parse(00:00:00)

*/

package custom_types

import (
	"database/sql/driver"
	"fmt"
	"strings"
	"time"

	"github.com/databrary/databrary/util"
	"github.com/pkg/errors"
)

type Segment struct {
	bounds string
	lower  *Interval
	upper  *Interval
}

func (s Segment) String() string {
	if s.bounds == "" {
		return "empty"
	} else if s.lower == nil && s.upper != nil {
		return fmt.Sprintf(`(,%s%c`, s.upper.String(), s.bounds[1])
	} else if s.lower != nil && s.upper == nil {
		return fmt.Sprintf(`%c%s,)`, s.bounds[0], s.lower.String())
	} else if s.lower == nil && s.upper == nil {
		return fmt.Sprint(`(,)`)
	} else {
		return fmt.Sprintf(
			`%c%s,%s%c`,
			s.bounds[0],
			s.lower.String(),
			s.upper.String(),
			s.bounds[1],
		)
	}
}

func NewSegment(lower, upper *Interval, bounds string) (Segment, error) {

	//if !set.NewSetFromSlice([]interface{}(strings.Split(bounds,""))).IsSubset(set.NewSet("[","]","(",")")) {
	//	errF := fmt.Sprintf("malformed bounds %s", bounds)
	//	return Segment{}, log.LogAndError(errF)
	//}
	// empty -> bounds = "", lower = nil, upper = nil
	if bounds == "" {
		if lower != nil || upper != nil {
			return Segment{}, errors.Errorf("empty segment with non-nil lower %s or upper %s", lower, upper)
		}
		return Segment{bounds: bounds, lower: lower, upper: upper}, nil
	}
	// [a,b] -> bounds = "[]", lower = a, upper = b
	// [a,b) -> bounds = "[)", lower = a, upper = b
	// (a,b] -> bounds = "(]", lower = a, upper = b
	// (a,b) -> bounds = "()", lower = a, upper = b
	if lower != nil && upper != nil {
		if !(lower.LE(*upper)) {
			return Segment{}, errors.Errorf(fmt.Sprintf("lower bound %s above upper bound %s", *lower, *upper))
		}
		// singleton check
		if upper.EQ(*lower) && bounds != "[]" {
			return Segment{}, errors.Errorf(fmt.Sprintf("wrong bounds %s for singleton", bounds))
		}

		return Segment{bounds: bounds, lower: lower, upper: upper}, nil
	}
	// (∞,a) -> bounds = "()", lower = nil, upper = a
	// (∞,a] -> bounds = "(]", lower = nil, upper = a
	// (∞,∞) -> bounds = "()", lower = nil, upper = nil
	if lower == nil {
		if bounds[0] != '(' {
			return Segment{}, errors.Errorf(fmt.Sprintf("nil lower %s with wrong bound %b", lower, bounds[0]))
		}
		if upper == nil && bounds[1] != ')' {
			return Segment{}, errors.Errorf(fmt.Sprintf("nil upper %s with wrong bound %b", upper, bounds[1]))
		}
		return Segment{bounds: bounds, lower: lower, upper: upper}, nil
	}
	// (a,∞) -> bounds = "()", lower = a, upper = nil
	// [a,∞) -> bounds = "[)", lower = a, upper = nil
	if upper == nil {
		if bounds[1] != ')' {
			return Segment{}, errors.Errorf(fmt.Sprintf("nil upper %s with wrong bound %b", upper, bounds[1]))
		}
		return Segment{bounds: bounds, lower: lower, upper: upper}, nil
	}
	panic(fmt.Sprintf("unreachable edge case %s %s %s", lower, upper, bounds))
	// unreachable - just to satisfy compiler
	return Segment{}, nil
}

func (s *Segment) Lower() *Interval {
	if s.bounds == "" {
		panic(fmt.Sprintf("no Lower for empty segment %s", s))
	}
	return s.lower
}

func (s *Segment) Upper() *Interval {
	if s.bounds == "" {
		panic(fmt.Sprintf("no Upper for empty segment %s", s))
	}
	return s.upper
}

func (s *Segment) IsEmpty() bool {
	return s.bounds == ""
}

func (s *Segment) IsInfLower() bool {
	return s.Lower() == nil
}

func (s *Segment) IsInfUpper() bool {
	return s.Upper() == nil
}

func (s *Segment) IsBounded() bool {
	return !(s.IsInfLower() || s.IsInfUpper())
}

func (s *Segment) IncLower() bool {
	if s.bounds != "" {
		panic(fmt.Sprintf("no Lower for empty segment %s", s))
	}
	return s.bounds[0] == '['
}

func (s *Segment) IncUpper() bool {
	if s.bounds != "" {
		panic(fmt.Sprintf("no Upper for empty segment %s", s))
	}
	return s.bounds[1] == ']'
}

// strictly less than
func (s *Segment) LowerLT(t *Segment) bool {
	if s.IsInfLower() {
		return true
	}
	if t.IsInfLower() {
		return false
	}
	return s.lower.LT(*t.lower)
}

// eg (a,x] and (a,y]
// or (a,x] and [a,y]
// this is wrt the infimum of each interval
// ie if the infima are equal then they're equal and hence LE
func (s *Segment) LowerLE(t *Segment) bool {
	if s.IsInfLower() {
		return true
	}
	if t.IsInfLower() {
		return false
	}
	return s.lower.LE(*t.lower)
}

func (s *Segment) LowerGE(t *Segment) bool {
	return !s.LowerLT(t)
}

func (s *Segment) LowerGT(t *Segment) bool {
	return !s.LowerLE(t)
}

func (s *Segment) UpperLT(t *Segment) bool {
	if s.IsInfUpper() {
		return false
	}
	if t.IsInfUpper() {
		return true
	}
	return s.upper.LT(*t.upper)
}

func (s *Segment) UpperLE(t *Segment) bool {
	if s.IsInfUpper() {
		return false
	}
	if t.IsInfUpper() {
		return true
	}
	return s.upper.LE(*t.upper)
}

func (s *Segment) UpperGE(t *Segment) bool {
	return !s.UpperLT(t)
}

func (s *Segment) UpperGT(t *Segment) bool {
	return !s.UpperLE(t)
}

func (s *Segment) Equal(t *Segment) bool {
	zero := Segment{}
	if *s == zero && *t == zero {
		return true
	}

	// lower both nil or eq
	l_eq := s.lower != nil && t.lower != nil && s.lower.EQ(*t.lower)
	l_eq = l_eq || (s.lower == nil && t.lower == nil)
	// upper both nil or eq
	u_eq := s.upper != nil && t.upper != nil && s.upper.EQ(*t.upper)
	u_eq = u_eq || (s.upper == nil && t.upper == nil)

	// bounds eq
	b_eq := s.bounds == t.bounds
	return l_eq && u_eq && b_eq
}

func (s *Segment) Contains(t *Segment) bool {
	if s.IsEmpty() {
		return false
	}
	if t.IsEmpty() || (s.IsInfUpper() && s.IsInfUpper()) {
		return true
	}
	return s.LowerLE(t) && s.UpperGE(t)
}

func (s *Segment) Duration() time.Duration {
	if !s.IsBounded() {
		errors.Errorf("unbounded %s", s)
	}
	return s.upper.interval - s.lower.interval
}

func (s *Segment) IsSingleton() bool {
	return s.lower == s.upper && s.IsBounded()
}

func (s *Segment) Shift(d time.Duration) {
	// shifting (,) should have no effect
	if !s.IsInfLower() {
		s.lower.interval = s.lower.interval + d
	}
	if !s.IsInfUpper() {
		s.upper.interval = s.upper.interval + d
	}
}

func (s *Segment) Minus(t *Segment) time.Duration {
	if !s.IsBounded() || !t.IsBounded() {
		errors.Errorf("unbounded s %s or t %s", s, t)
	}
	return s.Duration() - t.Duration()
}

// [18:16:54,18:22:01.008)
// note that milliseconds are optional
func parseSegment(segment string) (*Interval, *Interval, error) {
	trimmedSegmentString := segment[1 : len(segment)-1]
	begin_endAsString := strings.Split(trimmedSegmentString, ",")
	beginAsString, endAsString := begin_endAsString[0], begin_endAsString[1]
	var (
		endAsInterval   *Interval
		beginAsInterval *Interval
	)

	if len(beginAsString) == 0 {
		beginAsInterval = nil
	} else {
		i := Interval{}
		err := (&i).Scan([]byte(beginAsString))
		if err != nil {
			return nil, nil, err
		}
		beginAsInterval = &i
	}

	if len(endAsString) == 0 {
		endAsInterval = nil
	} else {
		i := Interval{}
		err := (&i).Scan([]byte(endAsString))
		if err != nil {
			return nil, nil, err
		}
		endAsInterval = &i
	}

	return beginAsInterval, endAsInterval, nil
}

// Scan implements the Scanner interface.
func (s *Segment) Scan(value interface{}) error {
	if value == nil {
		// this only happens for notifications
		return nil
	}
	segmentAsBytes, ok := value.([]byte)
	if !ok {
		return errors.Errorf("Could not convert %#v scanned Segment value to bytes", value)
	}
	segmentAsString := string(segmentAsBytes)
	if segmentAsString == "empty" {
		s.bounds = ""
		s.lower = nil
		s.upper = nil
		return nil
	}
	if segmentAsString == "(,)" {
		s.bounds = "()"
		s.lower = nil
		s.upper = nil
		return nil
	}
	var err error
	s.lower, s.upper, err = parseSegment(segmentAsString)
	if err != nil {
		return errors.Errorf("Could not parse %v into string. error %s", value, err)
	}
	s.bounds = string(segmentAsString[0]) + string(segmentAsString[len(segmentAsString)-1])
	return nil
}

func (s Segment) Value() (driver.Value, error) {
	// [00:18:34.15,00:22:46.909)
	segAsString := s.String()
	return []byte(segAsString), nil
}

type NullSegment struct {
	Segment Segment
	Valid   bool
}

func (ns *NullSegment) Scan(value interface{}) error {
	if value == nil {
		ns.Segment, ns.Valid = Segment{}, false
		return nil
	}

	err := ns.Segment.Scan(value)
	util.CheckOrFatalErr(err)
	ns.Valid = true
	return nil
}

func (ns NullSegment) Value() (driver.Value, error) {
	if !ns.Valid {
		return nil, nil
	}
	return ns.Segment.Value()
}

// this is not time.Duration but an alias for postgres interval hour to sec (3)
//type NullDuration struct {
//	time  time.Time
//	valid bool
//}
//
//func (d *NullDuration) Scan(value interface{}) error {
//	if value == nil {
//		d = &NullDuration{time.Time{}, false}
//		return nil
//	}
//	durationAsBytes, ok := value.([]byte)
//	if !ok {
//		return errors.Errorf("Could not convert %#v scanned Duration value to bytes", value)
//	}
//	durationAsString := string(durationAsBytes)
//	durationAsTime, err := parseStringToTime(durationAsString)
//	util.CheckOrFatalErr(err)
//	d.time, d.valid = durationAsTime, true
//	return nil
//}
//
//func (d NullDuration) Value() (driver.Value, error) {
//	if !d.valid {
//		return nil, nil
//	}
//	return []byte(d.time.Format(MIL_SEG_FORMAT)), nil
//}

func SegmentRandom() Segment {
	d1, _ := time.ParseDuration("1m")
	d2, _ := time.ParseDuration("2m")
	n1, n2 := NewInterval(d1), NewInterval(d2)

	seg, _ := NewSegment(&n1, &n2, "[]")
	return seg
}

func NullSegmentRandom() NullSegment {
	seg := SegmentRandom()
	return NullSegment{seg, true}
}
