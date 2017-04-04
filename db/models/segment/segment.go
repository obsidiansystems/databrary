package segment

import (
	"errors"
	"fmt"
	"strings"

	"database/sql/driver"
	"github.com/SaidinWoT/timespan"
	log "github.com/databrary/databrary/logging"
	"time"
)

const (
	MIL_SEG_FORMAT = "15:04:05.000"
	SEG_FORMAT     = "15:04:05"
)

type Segment struct {
	bounds  string
	lower   *time.Time
	upper   *time.Time
	segment *timespan.Span
}

func (s *Segment) String() string {
	if s.bounds == "" {
		return "empty"
	} else if s.lower == nil && s.upper != nil {
		return fmt.Sprintf(`(∞,%s%c`, s.upper, s.bounds[1])
	} else if s.lower != nil && s.upper == nil {
		return fmt.Sprintf(`%c%s,∞)`, s.bounds[0], s.upper)
	} else if s.lower == nil && s.upper == nil {
		return fmt.Sprint(`(∞,∞)`)
	} else {
		return fmt.Sprintf(`%c%s,%s%c`, s.bounds[0], s.lower, s.upper, s.bounds[1])
	}
}

func NewSegment(lower *time.Time, upper *time.Time, bounds string) (*Segment, error) {
	var seg *timespan.Span
	// empty -> bounds = "", lower = nil, upper = nil, segment = nil
	if bounds == "" {
		if lower != nil || upper != nil {
			errF := fmt.Sprintf("empty segment with non-nil lower %s or upper %s", lower, upper)
			log.Logger.Error(errF)
			return nil, errors.New(errF)
		}
		return &Segment{bounds: bounds, lower: lower, upper: upper, segment: seg}, nil
	}
	// [a,b] -> bounds = "[]", lower = a, upper = b, segment = Segment(a,b)
	// [a,b) -> bounds = "[)", lower = a, upper = b, segment = Segment(a,b)
	// (a,b] -> bounds = "(]", lower = a, upper = b, segment = Segment(a,b)
	// (a,b) -> bounds = "()", lower = a, upper = b, segment = Segment(a,b)
	if lower != nil && upper != nil {
		if !(upper.Sub(*lower).Nanoseconds() >= 0) {
			return nil, log.LogAndError(fmt.Sprintf("lower bound %s above upper bound %s", *lower, *upper))
		}
		realSeg := timespan.New(*lower, upper.Sub(*lower))
		return &Segment{bounds: bounds, lower: lower, upper: upper, segment: &realSeg}, nil
	}
	// (∞,a) -> bounds = "()", lower = nil, upper = a, segment = nil
	// (∞,a] -> bounds = "(]", lower = nil, upper = a, segment = nil
	// (∞,∞) -> bounds = "()", lower = nil, upper = nil
	if lower == nil {
		if bounds[0] != '(' {
			return nil, log.LogAndError(fmt.Sprintf("nil lower %s with wrong bound %s", lower, bounds[0]))
		}
		if upper == nil && bounds[1] != ')' {
			return nil, log.LogAndError(fmt.Sprintf("nil upper %s with wrong bound %s", upper, bounds[1]))
		}
		return &Segment{bounds: bounds, lower: lower, upper: upper, segment: seg}, nil
	}
	// (a,∞) -> bounds = "()", lower = a, upper = nil, segment = nil
	// [a,∞) -> bounds = "[)", lower = a, upper = nil, segment = nil
	if upper == nil {
		if bounds[1] != ')' {
			return nil, log.LogAndError(fmt.Sprintf("nil upper %s with wrong bound %s", upper, bounds[1]))
		}
		return &Segment{bounds: bounds, lower: lower, upper: upper, segment: seg}, nil
	}
	log.Logger.Errorf("unreachable edge case %s %s %s", lower, upper, bounds)
	panic(fmt.Sprintf("unreachable edge case %s %s %s", lower, upper, bounds))
}

func (s *Segment) Lower() *time.Time {
	if s.bounds == "" {
		panic(fmt.Sprintf("no Lower for empty segment %s", s))
	}
	return s.lower
}

func (s *Segment) Upper() *time.Time {
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
	return s.IsInfLower() || s.IsInfUpper()
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

const (
	lt = iota
	eq
	gt
)

// this is wrt the infimum of each interval
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

func (s *Segment) Contains(t *Segment) bool {
	if s.IsEmpty() {
		return false
	}

	if t.IsEmpty() || (s.IsInfUpper() && s.IsInfUpper()) {
		return true
	}
	return s.LowerLE(t) && s.UpperGE(t)
}

func parseTimeToString(timeAsString string) (*time.Time, error) {

	// i'm using SEG_FORMAT here and MIL_SEG_FORMAT up there because
	// for some reason this parses 00:18:34.15 correctly but i want
	// to keep milliseconds when serializing
	timeAsTime, err := time.Parse(SEG_FORMAT, timeAsString)
	if err != nil {
		log.Logger.Errorf("couldn't parse %s as time. error %s", timeAsString, err)
		return nil, err
	}
	return &timeAsTime, nil
}

// [18:16:54,18:22:01.008)
// note that milliseconds are optional
func parseSegment(segment string) (*time.Time, *time.Time, error) {
	trimmedSegmentString := segment[1 : len(segment)-1]
	begin_endAsString := strings.Split(trimmedSegmentString, ",")
	beginAsString, endAsString := begin_endAsString[0], begin_endAsString[1]

	// (,) TODO: check to see whether value == nil handles this
	if len(beginAsString) == 0 && len(endAsString) == 0 {
		return nil, nil, nil
	}
	// this is probably not possible but just in case
	if len(beginAsString) == 0 || len(endAsString) == 0 {
		err := errors.New(fmt.Sprintf("malformed segment %s", segment))
		log.Logger.Errorf("%s", err)
		return nil, nil, err
	}

	beginAsTime, err := parseTimeToString(beginAsString)
	if err != nil {
		return nil, nil, err
	}
	endAsTime, err := parseTimeToString(endAsString)
	if err != nil {
		return nil, nil, err
	}
	return beginAsTime, endAsTime, nil
}

// Scan implements the Scanner interface.
func (s *Segment) Scan(value interface{}) error {
	s.segment = nil
	if value == nil {
		return nil
	}
	segmentAsBytes, ok := value.([]byte)
	if !ok {
		log.Logger.Errorf("Could not convert %#v scanned Segment value to bytes", value)
		return errors.New("Could not convert scanned Segment value to bytes")
	}
	segmentAsString := string(segmentAsBytes)
	if segmentAsString == "empty" {
		log.Logger.Errorf("empty segment %#v scanned", value)
		return errors.New("empty segment")
	}
	if segmentAsString == "(,)" {
		return nil
	}
	begin, end, err := parseSegment(segmentAsString)
	if err != nil {
		msg := fmt.Sprintf("Could not parse %v into string. error", value, err)
		log.Logger.Error(msg)
		return errors.New(msg)
	}
	seg := timespan.New(*begin, end.Sub(*begin))
	s.segment = &seg

	return nil
}

func (s Segment) Value() (driver.Value, error) {
	// [00:18:34.15,00:22:46.909)
	segAsString := s.String()
	return []byte(segAsString), nil
}
