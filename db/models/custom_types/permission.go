package custom_types

import (
	"database/sql"
	"database/sql/driver"

	"github.com/databrary/databrary/logging"
)

// Permission enum
type Permission sql.NullString

// Levels of access parties can have to the site data.
var (
	PermNONE   Permission = Permission{"NONE", true}
	PermPUBLIC Permission = Permission{"PUBLIC", true}
	PermSHARED Permission = Permission{"SHARED", true}
	PermREAD   Permission = Permission{"READ", true}
	PermEDIT   Permission = Permission{"EDIT", true}
	PermADMIN  Permission = Permission{"ADMIN", true}
)

func (p Permission) Value() (driver.Value, error) {
	// value needs to be a base driver.Value type
	// such as bool.
	if p.Valid {
		return p.String, nil
	} else {
		return nil, nil
	}
}

func (p *Permission) Scan(value interface{}) error {
	if value == nil {
		p.String, p.Valid = "", false
		return nil
	}
	if val, err := driver.String.ConvertValue(value); err == nil {
		if val, ok := val.([]byte); ok {
			*p = Permission{string(val), true}
			return nil
		}
	}
	return logging.LogAndError("failed to scan Permission")
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*Permission)(nil)
var _ driver.Valuer = PermADMIN
