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

package segment

import (
	"database/sql/driver"
	"fmt"
	"strings"
	"time"

	log "github.com/databrary/databrary/logging"
)

const (
	MIL_SEG_FORMAT = "15:04:05.000"
	SEG_FORMAT     = "15:04:05"
)

type Segment struct {
	bounds string
	lower  *time.Time
	upper  *time.Time
}

func (s *Segment) String() string {
	if s.bounds == "" {
		return "empty"
	} else if s.Lower() == nil && s.Upper() != nil {
		return fmt.Sprintf(`(,%s%c`, s.Upper().Format(MIL_SEG_FORMAT), s.bounds[1])
	} else if s.Lower() != nil && s.Upper() == nil {
		return fmt.Sprintf(`%c%s,)`, s.bounds[0], s.Lower().Format(MIL_SEG_FORMAT))
	} else if s.Lower() == nil && s.Upper() == nil {
		return fmt.Sprint(`(,)`)
	} else {
		return fmt.Sprintf(
			`%c%s,%s%c`,
			s.bounds[0],
			s.Lower().Format(MIL_SEG_FORMAT),
			s.Upper().Format(MIL_SEG_FORMAT),
			s.bounds[1],
		)
	}
}

func NewSegment(lower *time.Time, upper *time.Time, bounds string) (*Segment, error) {
	// empty -> bounds = "", lower = nil, upper = nil
	if bounds == "" {
		if lower != nil || upper != nil {
			errF := fmt.Sprintf("empty segment with non-nil lower %s or upper %s", lower, upper)

			return nil, log.LogAndError(errF)
		}
		return &Segment{bounds: bounds, lower: lower, upper: upper}, nil
	}
	// [a,b] -> bounds = "[]", lower = a, upper = b
	// [a,b) -> bounds = "[)", lower = a, upper = b
	// (a,b] -> bounds = "(]", lower = a, upper = b
	// (a,b) -> bounds = "()", lower = a, upper = b
	if lower != nil && upper != nil {
		if !(upper.Sub(*lower).Nanoseconds() >= 0) {
			return nil, log.LogAndError(fmt.Sprintf("lower bound %s above upper bound %s", *lower, *upper))
		}
		// singleton check
		if upper.Sub(*lower) == 0 && bounds != "[]" {
			return nil, log.LogAndError(fmt.Sprintf("wrong bounds %s for singleton", bounds))
		}

		return &Segment{bounds: bounds, lower: lower, upper: upper}, nil
	}
	// (∞,a) -> bounds = "()", lower = nil, upper = a
	// (∞,a] -> bounds = "(]", lower = nil, upper = a
	// (∞,∞) -> bounds = "()", lower = nil, upper = nil
	if lower == nil {
		if bounds[0] != '(' {
			return nil, log.LogAndError(fmt.Sprintf("nil lower %s with wrong bound %b", lower, bounds[0]))
		}
		if upper == nil && bounds[1] != ')' {
			return nil, log.LogAndError(fmt.Sprintf("nil upper %s with wrong bound %b", upper, bounds[1]))
		}
		return &Segment{bounds: bounds, lower: lower, upper: upper}, nil
	}
	// (a,∞) -> bounds = "()", lower = a, upper = nil
	// [a,∞) -> bounds = "[)", lower = a, upper = nil
	if upper == nil {
		if bounds[1] != ')' {
			return nil, log.LogAndError(fmt.Sprintf("nil upper %s with wrong bound %b", upper, bounds[1]))
		}
		return &Segment{bounds: bounds, lower: lower, upper: upper}, nil
	}
	log.Logger.Panic(fmt.Sprintf("unreachable edge case %s %s %s", lower, upper, bounds))
	// unreachable
	return nil, nil
}

func (s *Segment) Lower() *time.Time {
	if s.bounds == "" {
		log.Logger.Panic(fmt.Sprintf("no Lower for empty segment %s", s))
	}
	return s.lower
}

func (s *Segment) Upper() *time.Time {
	if s.bounds == "" {
		log.Logger.Panic(fmt.Sprintf("no Upper for empty segment %s", s))
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
		log.Logger.Panic(fmt.Sprintf("no Lower for empty segment %s", s))
	}
	return s.bounds[0] == '['
}

func (s *Segment) IncUpper() bool {
	if s.bounds != "" {
		log.Logger.Panic(fmt.Sprintf("no Upper for empty segment %s", s))
	}
	return s.bounds[1] == ']'
}

const (
	lt = iota
	eq
	gt
)

// left to right
func finiteOrder(s, t *time.Time) int {
	diff := t.Sub(*s)
	if diff > 0 {
		return lt
	} else if diff == 0 {
		return eq
	} else {
		return gt
	}
}

func (s *Segment) LowerLT(t *Segment) bool {
	if s.IsInfLower() {
		return true
	}
	if t.IsInfLower() {
		return false
	}

	sl := s.Lower()
	tl := t.Lower()
	switch finiteOrder(sl, tl) {
	case lt:
		return true
	case eq, gt:
		return false
	default:
		return false
	}
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
	sl := s.Lower()
	tl := t.Lower()
	return finiteOrder(sl, tl) != gt
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
	su := s.Upper()
	tu := t.Upper()
	switch finiteOrder(su, tu) {
	case lt:
		return true
	case eq, gt:
		return false
	default:
		return false
	}
}

func (s *Segment) UpperLE(t *Segment) bool {
	if s.IsInfUpper() {
		return false
	}
	if t.IsInfUpper() {
		return true
	}
	su := s.Upper()
	tu := s.Upper()
	return finiteOrder(su, tu) != gt
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
	eqTime := func(t1 *time.Time, t2 *time.Time) bool {
		if t1 == nil {
			return t2 == nil
		}
		if t2 == nil {
			return false
		}
		return t1.Equal(*t2)
	}
	return s.bounds == t.bounds && eqTime(s.Lower(), t.Lower()) && eqTime(s.Upper(), t.Upper())
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
		log.Logger.Panic(log.LogAndError(fmt.Sprintf("unbounded %s", s)))
	}
	sl := s.Lower()
	su := s.Upper()
	return su.Sub(*sl)
}

func (s *Segment) IsSingleton() bool {
	return s.Upper().Sub(*s.Lower()) == 0 && s.IsBounded()
}

func (s *Segment) Shift(d time.Duration) {
	// shifting (,) should have no effect
	if !s.IsInfLower() {
		newLower := s.lower.Add(d)
		s.lower = &newLower
	}
	if !s.IsInfUpper() {
		newUpper := s.upper.Add(d)
		s.upper = &newUpper
	}
}

func (s *Segment) Minus(t *Segment) time.Duration {
	if !s.IsBounded() || !t.IsBounded() {
		log.Logger.Panic(log.LogAndError(fmt.Sprintf("unbounded s %s or t %s", s, t)))
	}
	return s.Duration() - t.Duration()
}

//in go seconds are counted relative to January 1, year 1 00:00:00 UTC
//while in postgres they're counted relative to the start of the epoch (unix epoch) i.e 0
//so 00:00:00 is parsed as -31622400 seconds or 31622400000000000 nanoseconds (1 year before january 1 01.
//EPOCHSHIFT shifts 00:00:00 forward by simply addding 31622400000000000 to time.Parse(00:00:00)
const EPOCHSHIFT = time.Duration(31622400000000000)

//func epochShift() time.Duration {
//	timeAsString := "00:00:00"
//	timeAsTime, _ := time.Parse(SEG_FORMAT, timeAsString)
//	d := time.Duration(time.Time{}.Sub(timeAsTime).Nanoseconds())
//	return d
//}

func parseTimeToString(timeAsString string) (*time.Time, error) {

	// i'm using SEG_FORMAT here and MIL_SEG_FORMAT up there because
	// for some reason this parses 00:18:34.15 correctly but i want
	// to explicitly keep .000 when serializing to the db
	timeAsTime, err := time.Parse(SEG_FORMAT, timeAsString)
	timeAsTime = timeAsTime.Add(EPOCHSHIFT)
	if err != nil {
		return nil, log.LogAndError(fmt.Sprintf("couldn't parse %s as time. error %s", timeAsString, err))
	}
	return &timeAsTime, nil
}

// [18:16:54,18:22:01.008)
// note that milliseconds are optional
func parseSegment(segment string) (*time.Time, *time.Time, error) {
	trimmedSegmentString := segment[1 : len(segment)-1]
	begin_endAsString := strings.Split(trimmedSegmentString, ",")
	beginAsString, endAsString := begin_endAsString[0], begin_endAsString[1]

	if len(beginAsString) == 0 && len(endAsString) == 0 {
		// (,)
		return nil, nil, nil
	}
	var (
		endAsTime   *time.Time
		beginAsTime *time.Time
		err         error
	)
	if len(beginAsString) != 0 {
		beginAsTime, err = parseTimeToString(beginAsString)
	}
	if err != nil {
		return nil, nil, err
	}
	if len(endAsString) != 0 {
		endAsTime, err = parseTimeToString(endAsString)
	}
	if err != nil {
		return nil, nil, err
	}

	return beginAsTime, endAsTime, nil
}

// Scan implements the Scanner interface.
func (s *Segment) Scan(value interface{}) error {
	if value == nil {
		// this only happens for notifications
		return nil
	}
	segmentAsBytes, ok := value.([]byte)
	if !ok {
		return log.LogAndError(fmt.Sprintf("Could not convert %#v scanned Segment value to bytes", value))
	}
	segmentAsString := string(segmentAsBytes)
	if segmentAsString == "empty" {
		//s = &Segment{bounds: "", lower: nil, upper: nil}
		s.bounds = ""
		s.lower = nil
		s.upper = nil
		return nil
	}
	if segmentAsString == "(,)" {
		//s = &Segment{bounds: "()", lower: nil, upper: nil}
		s.bounds = "()"
		s.lower = nil
		s.upper = nil
		return nil
	}
	begin, end, err := parseSegment(segmentAsString)
	if err != nil {
		return log.LogAndError(fmt.Sprintf("Could not parse %v into string. error %s", value, err))
	}
	s.bounds = string(segmentAsString[0]) + string(segmentAsString[len(segmentAsString)-1])
	s.lower = begin
	s.upper = end

	return nil
}

func (s Segment) Value() (driver.Value, error) {
	// [00:18:34.15,00:22:46.909)
	segAsString := s.String()
	return []byte(segAsString), nil
}
