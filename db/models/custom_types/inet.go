package custom_types

import (
	"database/sql"
	"database/sql/driver"
	"errors"
	"net"
)

// A wrapper for transferring Inet values back and forth easily.
type Inet net.IP

// Scan implements the Scanner interface.
func (i *Inet) Scan(value interface{}) error {
	*i = Inet(net.IP{})
	if value == nil {
		return errors.New("scanned NULL Inet (did you mean to use NullInet?)")
	}
	ipAsBytes, ok := value.([]byte)
	if !ok {
		return errors.New("Could not convert scanned Inet value to bytes")
	}
	parsedIP := net.ParseIP(string(ipAsBytes))
	if parsedIP == nil {
		return errors.New("parsed NULL ip (did you mean to use NullInet?)")
	}
	*i = Inet(parsedIP)
	return nil
}

// Value implements the driver Valuer interface. Note if i.Valid is false
// or i.IP is nil the database column value will be set to NULL.
func (i Inet) Value() (driver.Value, error) {
	if i == nil {
		return nil, errors.New("value nil Inet (did you mean to use NullInet?)")
	}
	return net.IP(i).String(), nil
}

// Nullable Inet. Just a wrapper around Inet.
type NullInet struct {
	Inet  Inet
	Valid bool
}

// Implements Scanner interface.
// This is what is used to convert a column of type action from a postgres query
// into this Go type. The argument has the []byte representation of the column.
// Null columns scan to nv.Valid == false.
func (ni *NullInet) Scan(value interface{}) error {
	if value == nil {
		ni.Inet, ni.Valid = Inet{}, false
		return nil
	}
	err := ni.Inet.Scan(value)
	if err != nil {
		ni.Valid = false
		return err
	} else {
		ni.Valid = true
		return nil
	}
}

// Implements Valuer interface
// This is what is used to convert a  Go type action to a postgres type.
// Valid == false turns into a Null value.
func (ni NullInet) Value() (driver.Value, error) {
	if !ni.Valid {
		return nil, nil
	}
	return ni.Inet.Value()
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*Inet)(nil)
var _ driver.Valuer = Inet{}

var _ sql.Scanner = (*NullInet)(nil)
var _ driver.Valuer = NullInet{}
