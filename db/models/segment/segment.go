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

// A wrapper for transferring Inet values back and forth easily.
// Valid == true and Segment == nil <=> (,)
type Segment struct {
	Segment *timespan.Span
	Valid   bool
}

func (s *Segment) String() string {
	return fmt.Sprintf(
		"[%s,%s)",
		s.Segment.Start().Format(MIL_SEG_FORMAT),
		s.Segment.End().Format(MIL_SEG_FORMAT),
	)
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
	s.Segment = nil
	s.Valid = false
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
		s.Valid = true
		return nil
	}
	begin, end, err := parseSegment(segmentAsString)
	if err != nil {
		msg := fmt.Sprintf("Could not parse %v into string. error", value, err)
		log.Logger.Error(msg)
		return errors.New(msg)
	}
	seg := timespan.New(*begin, end.Sub(*begin))
	s.Segment = &seg
	s.Valid = true

	return nil
}

func (s Segment) Value() (driver.Value, error) {
	if s.Valid == false || s.Segment == nil {
		return nil, nil
	}
	// [00:18:34.15,00:22:46.909)
	segAsString := s.String()
	return []byte(segAsString), nil
}
