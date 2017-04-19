package custom_types

import (
	"database/sql"
	"database/sql/driver"

	"github.com/databrary/databrary/logging"
)

// Action Enum
type Action sql.NullString

// The various activities for which we keep audit records (in audit or a derived table).
var (
	ActionATTEMPT   Action = Action{"attempt", true}
	ActionOPEN      Action = Action{"open", true}
	ActionCLOSE     Action = Action{"close", true}
	ActionADD       Action = Action{"add", true}
	ActionCHANGE    Action = Action{"change", true}
	ActionREMOVE    Action = Action{"remove", true}
	ActionSUPERUSER Action = Action{"superuser", true}
)

func (act Action) Value() (driver.Value, error) {
	if !act.Valid {
		return nil, nil
	}
	// value needs to be a base driver.Value type
	// such as bool.
	return act.String, nil
}

func (act *Action) Scan(value interface{}) error {
	if value == nil {
		act.String, act.Valid = "", false
		return nil
	}
	if exposure_val, err := driver.String.ConvertValue(value); err == nil {
		if v, ok := exposure_val.([]byte); ok {
			*act = Action{string(v), true}
			return nil
		}
	}
	return logging.LogAndError("failed to scan Action")
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*Action)(nil)
var _ driver.Valuer = ActionSUPERUSER
