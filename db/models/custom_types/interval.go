package custom_types

import (
	"database/sql/driver"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/pkg/errors"
)

type Interval struct {
	interval time.Duration
}

func (i *Interval) Interval() time.Duration {
	return i.interval
}

func NewInterval(d time.Duration) Interval {
	isNeg := d < 0
	if isNeg {
		d *= -1
	}
	nanos := d.Nanoseconds()
	// if microseconds
	if nanos%time.Millisecond.Nanoseconds() != 0 {
		// find microseconds
		micros := nanos % time.Millisecond.Nanoseconds()
		mills := nanos / time.Millisecond.Nanoseconds()
		// find leading digit of microseconds, i.e. how many hundred-thousand nanoseconds
		lead := micros / (100 * time.Microsecond.Nanoseconds())
		// round up if necessary
		if lead >= 5 {
			mills += 1
		}
		nanos = mills * 1e6
	}
	if isNeg {
		nanos *= -1
	}
	return Interval{time.Duration(nanos)}

}

// convenience method
func NewIntervalFromString(inter string) Interval {
	i := Interval{}
	i.Scan([]byte(inter))
	return i
}

// Less than
func (i Interval) LT(j Interval) bool {
	return i.interval < j.interval
}

// Equal
func (i Interval) EQ(j Interval) bool {
	return i.interval == j.interval
}

// Less or Equal
func (i Interval) LE(j Interval) bool {
	return i.LT(j) || i.EQ(j)
}

// Greater than
func (i Interval) GT(j Interval) bool {
	return !i.LE(j)
}

// Greater than or equal
func (i Interval) GE(j Interval) bool {
	return i.GT(j) || i.EQ(j)
}

func (i *Interval) String() string {
	isNeg := i.interval < 0
	if isNeg {
		i.interval *= -1
	}
	hours := int(i.interval.Hours())
	minutes := int(i.interval.Minutes() - float64(hours)*60)
	seconds := i.interval.Seconds() - float64(minutes)*60 - float64(hours)*3600
	s := fmt.Sprintf("%d:%d:%.3f", hours, minutes, seconds)
	if isNeg {
		s = "-" + s
	}
	return s
}

// Scan satisfies the database/sql.Scanner interface for Interval.
func (i *Interval) Scan(value interface{}) error {
	intervalAsBytes, ok := value.([]byte)
	if !ok {
		return errors.Errorf("%s couldn't be cast to byte", value)
	}
	intervalAsString := string(intervalAsBytes)
	chunks := strings.Split(intervalAsString, ":")
	hoursAsString := chunks[0]
	isNeg := hoursAsString[0] == '-'
	var (
		err   error
		hours int
	)
	if isNeg {
		hours, err = strconv.Atoi(hoursAsString[1:])
	} else {
		hours, err = strconv.Atoi(hoursAsString)
	}
	if err != nil {
		return errors.Wrapf(err, "failed to convert %s to int hours", chunks[0])
	}

	minutes, err := strconv.Atoi(chunks[1])
	if err != nil {
		return errors.Wrapf(err, "failed to convert %s to int minutes", chunks[1])
	}
	seconds, err := strconv.ParseFloat(chunks[2], 64)
	if err != nil {
		return errors.Wrapf(err, "failed to convert %s to float seconds", chunks[2])
	}
	s := fmt.Sprintf("%dh%dm%.3fs", hours, minutes, seconds)
	if isNeg {
		s = "-" + s
	}
	i.interval, err = time.ParseDuration(s)
	if err != nil {
		return errors.Errorf("failed to parse duration from %s", s)
	}
	return nil
}

// Value satisfies the sql/driver.Valuer interface for Interval.
func (i Interval) Value() (driver.Value, error) {
	return []byte(i.String()), nil
}

// Nullable Interval. Just a wrapper around Interval.
type NullInterval struct {
	Interval Interval
	Valid    bool
}

// Implements Scanner interface.
// This is what is used to convert a column of type action from a postgres query
// into this Go type. The argument has the []byte representation of the column.
// Null columns scan to nv.Valid == false.
func (nv *NullInterval) Scan(value interface{}) error {
	if value == nil {
		nv.Interval, nv.Valid = Interval{}, false
		return nil
	}
	err := nv.Interval.Scan(value)
	if err != nil {
		nv.Valid = false
		return err
	} else {
		nv.Valid = true
		return nil
	}
}

// Implements Valuer interface
// This is what is used to convert a  Go type action to a postgres type.
// Valid == false turns into a Null value.
func (nv NullInterval) Value() (driver.Value, error) {
	if !nv.Valid {
		return nil, nil
	}
	return nv.Interval.Value()
}

// This function is used for testing SQLBoiler models, i.e. randomization
// for models that have a Interval column.
// Obviously it's not random but it doesn't really need to be anyway.
func IntervalRandom() Interval {
	d1, _ := time.ParseDuration("1m")
	return Interval{d1}
}

// This function is used for testing SQLBoiler models, i.e. randomization
// for models that have a NullInterval column.
// Obviously it's not random but it doesn't really need to be anyway.
func NullIntervalRandom() NullInterval {
	seg := IntervalRandom()
	return NullInterval{seg, true}
}
