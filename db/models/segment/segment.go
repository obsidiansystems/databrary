package segment

import (
	//"net"
	"errors"
	//"database/sql/driver"
	"fmt"
	"strings"
	//"time"

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
	bounds *string
	lower *time.Time
	upper *time.Time
	segment *timespan.Span
}


func (s *Segment) String() string {
	if s.bounds == nil {
		return "empty"
	} else if s.lower == nil && s.upper != nil {
		return fmt.Sprintf(`(∞,%s%c`, s.upper, (*s.bounds)[1])
	} else if s.lower != nil && s.upper == nil {
		return fmt.Sprintf(`%c%s,∞)`, (*s.bounds)[0], s.upper)
	} else if s.lower == nil && s.upper == nil {
		return fmt.Sprint(`(∞,∞)`)
	} else {
		return fmt.Sprintf(`%c%s,%s%c`, (*s.bounds)[0], s.lower, s.upper, (*s.bounds)[1])
	}
}


// bounds nil is empty (also make sure lower and upper are nil)
// infinite ranges include endpoints
func NewSegment(lower *time.Time, upper *time.Time, bounds *string) (*Segment, error) {
	var seg timespan.Span
	if lower != nil && upper != nil {
		seg = timespan.New(*lower, upper.Sub(*lower))
	}

	if lower == nil && (*bounds)[0] == '[' {
		log.Logger.Error("lower unbounded necessitates ( in bounds")
		return nil, errors.New("lower unbounded necessitates ( in bounds")
	}

	if upper == nil && (*bounds)[1] == ']' {
		log.Logger.Error("upper unbounded necessitates ) in bounds")
		return nil, errors.New("upper unbounded necessitates ) in bounds")
	}

	return &Segment{
		bounds: bounds,
		lower: lower,
		upper: upper,
		segment: &seg,
	}, nil
}


func (s *Segment) Lower() (*time.Time, error) {
	if s.lower == nil && s.bounds == nil {
		log.Logger.Errorf("no lower bound empty Segment %s", s)
		return nil, errors.New(fmt.Sprintf("no lower bound empty Segment %s", s))
	} else {
		return s.lower, nil
	}
}


func (s *Segment) Upper() (*time.Time, error) {
	if s.upper == nil && s.bounds == nil {
		log.Logger.Errorf("no upper bound empty Segment %s", s)
		return nil, errors.New(fmt.Sprintf("no upper bound empty Segment %s", s))
	} else {
		return s.upper, nil
	}
}


func (s *Segment) IsEmpty() (bool, error) {
	if s.bounds != nil {
		if s.lower == nil && s.upper == nil{
			log.Logger.Errorf("non-nil bounds, nil bound Segment %s", s)
			return false, errors.New(fmt.Sprintf("non-nil bounds, nil bound Segment %s", s))
		}
		return false, nil
	} else {
		if s.lower != nil || s.upper != nil {
			log.Logger.Errorf("nil bounds, non-nil bound Segment %s", s)
			return false, errors.New(fmt.Sprintf("nil bounds, non-nil bound Segment %s", s))
		}
		return true, nil
	}
}



// true is lower bound is infinite
func (s *Segment) InfLower() (bool, error) {
	if s.bounds != nil {
		return s.lower == nil, nil
	} else {
		log.Logger.Errorf("no lower bound empty Segment %s", s)
		return false, errors.New(fmt.Sprintf("no lower bound empty Segment %s", s))
	}
}


func (s *Segment) InfUpper() (bool, error) {
	if s.bounds != nil {
		return s.upper == nil, nil
	} else {
		log.Logger.Errorf("no upper bound empty Segment %s", s)
		return false, errors.New(fmt.Sprintf("no upper bound empty Segment %s", s))
	}
}


// true if lower bound is included
func (s *Segment) IncLower() (bool, error) {
	if s.bounds != nil {
		if s.lower == nil {
			return true, nil
		} else {
			return (*s.bounds)[0] == '[', nil
		}
	} else {
		log.Logger.Errorf("no lower bound empty Segment %s", s)
		return false, errors.New(fmt.Sprintf("no lower bound empty Segment %s", s))
	}
}


// true if upper bound is included
func (s *Segment) IncUpper() (bool, error) {
	if s.bounds != nil {
		if s.upper == nil {
			return true, nil
		} else {
			return (*s.bounds)[1] == ']', nil
		}
	} else {
		log.Logger.Errorf("no upper bound empty Segment %s", s)
		return false, errors.New(fmt.Sprintf("no upper bound empty Segment %s", s))
	}
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
