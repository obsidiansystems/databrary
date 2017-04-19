package custom_types

import (
	"database/sql"
	"database/sql/driver"
	"time"
)

type Date struct {
	date  time.Time
	valid bool
}

func NewDate(date time.Time, valid bool) Date {
	return Date{
		// dates are truncated in postgres to nearest day and return
		// with no timezone
		date:  date.Truncate(24 * time.Hour).In(time.FixedZone("", 0)),
		valid: valid,
	}
}

func (ns *Date) Valid() bool {
	return ns.valid
}

func (ns *Date) Time() time.Time {
	return ns.date
}

// Scan implements the Scanner interface.
func (ns *Date) Scan(value interface{}) error {
	if value == nil {
		ns.date, ns.valid = time.Time{}, false
		return nil
	}
	ns.valid = true
	ns.date = value.(time.Time)
	return nil
}

// Value implements the driver Valuer interface.
func (ns Date) Value() (driver.Value, error) {
	if !ns.valid {
		return nil, nil
	}
	return ns.date, nil
}

// checking to make sure interface is implemented
var _ sql.Scanner = (*Date)(nil)
var _ driver.Valuer = Date{}
