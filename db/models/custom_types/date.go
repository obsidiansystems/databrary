package custom_types

import (
	"database/sql"
	"database/sql/driver"
	"time"

	"github.com/databrary/databrary/logging"
)

type Date struct {
	// not alias to enforce truncation
	date time.Time // not public to enforce truncation
}

func NewDate(date time.Time, valid bool) Date {
	return Date{
		// dates are truncated in postgres to nearest day and return
		// with no timezone
		date: date.Truncate(24 * time.Hour).In(time.FixedZone("", 0)),
	}
}

func (ns *Date) Time() time.Time {
	return ns.date
}

func (d *Date) Scan(value interface{}) error {
	if value == nil {
		d.date = time.Time{}
		return logging.LogAndError("scanned NULL Date (did you mean to use NullDate")
	}
	d.date = value.(time.Time)
	return nil
}

func (d Date) Value() (driver.Value, error) {
	return d.date, nil
}

type NullDate struct {
	Date  Date
	Valid bool
}

// Scan implements the Scanner interface.
func (nd *NullDate) Scan(value interface{}) error {
	if value == nil {
		nd.Date, nd.Valid = Date{time.Time{}}, false
		return nil
	}
	err := nd.Date.Scan(value)
	if err != nil {
		nd.Valid = false
		return err
	} else {
		nd.Valid = true
		return nil
	}
}

// Value implements the driver Valuer interface.
func (nd NullDate) Value() (driver.Value, error) {
	if !nd.Valid {
		return nil, nil
	}
	return nd.Date.Value()
}

// checking to make sure interface is implemented
var _ sql.Scanner = (*Date)(nil)
var _ driver.Valuer = Date{}

var _ sql.Scanner = (*NullDate)(nil)
var _ driver.Valuer = NullDate{}
