package varchar

import (
	"database/sql"
	"database/sql/driver"
	"strings"
)

type VarChar struct {
	string string
	valid  bool // Valid is true if String is not NULL
}

func NewVarChar(string string, valid bool) VarChar {
	return VarChar{
		string: strings.Trim(string, " "),
		valid:  valid,
	}
}

func (ns *VarChar) Valid() bool {
	return ns.valid
}

func (ns *VarChar) String() string {
	return strings.Trim(ns.string, " ")
}

// Scan implements the Scanner interface.
func (ns *VarChar) Scan(value interface{}) error {
	if value == nil {
		ns.string, ns.valid = "", false
		return nil
	}
	ns.valid = true
	ns.string = strings.Trim(value.(string), " ")
	return nil
}

// Value implements the driver Valuer interface.
func (ns VarChar) Value() (driver.Value, error) {
	if !ns.valid {
		return nil, nil
	}
	return ns.String(), nil
}

// checking to make sure interface is implemented
var _ sql.Scanner = (*VarChar)(nil)
var _ driver.Valuer = VarChar{}
