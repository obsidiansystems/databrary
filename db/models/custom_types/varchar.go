package custom_types

import (
	"database/sql"
	"database/sql/driver"
	"strings"

	"github.com/databrary/databrary/logging"
)

type VarChar struct {
	// not alias to enforce trimming
	string string // not public because need to enforce trim
}

func NewVarChar(string string, valid bool) VarChar {
	return VarChar{
		string: strings.Trim(string, " "),
	}
}

func (ns *VarChar) String() string {
	return strings.Trim(ns.String(), " ")
}

// Scan implements the Scanner interface.
func (ns *VarChar) Scan(value interface{}) error {
	ns.string = ""
	if value == nil {
		return logging.LogAndError("scanned NULL VarChar (did you mean to use NullVarChar)")
	}
	ns.string = value.(string)
	return nil
}

// Value implements the driver Valuer interface.
func (ns VarChar) Value() (driver.Value, error) {
	return strings.Trim(ns.String(), " "), nil
}

type NullVarChar struct {
	VarChar VarChar
	Valid   bool
}

func (nv *NullVarChar) Scan(value interface{}) error {
	if value == nil {
		nv.VarChar, nv.Valid = VarChar{""}, false
		return nil
	}
	err := nv.VarChar.Scan(value)
	if err != nil {
		nv.Valid = false
		return err
	} else {
		nv.Valid = true
		return nil
	}
}

func (nv NullVarChar) Value() (driver.Value, error) {
	if !nv.Valid {
		return nil, nil
	}
	return nv.VarChar.Value()
}

// checking to make sure interface is implemented
var _ sql.Scanner = (*VarChar)(nil)
var _ driver.Valuer = VarChar{}

var _ sql.Scanner = (*NullVarChar)(nil)
var _ driver.Valuer = NullVarChar{}
