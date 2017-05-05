package custom_types

import (
	"database/sql"
	"database/sql/driver"

	"github.com/databrary/databrary/logging"
	set "github.com/deckarep/golang-set"
)

// Action Enum
type Action string

// The various activities for which we keep audit records (in audit or a derived table).
// TODO: uppercase in schema
const (
	ActionATTEMPT   Action = Action("attempt")
	ActionOPEN      Action = Action("open")
	ActionCLOSE     Action = Action("close")
	ActionADD       Action = Action("add")
	ActionCHANGE    Action = Action("change")
	ActionREMOVE    Action = Action("remove")
	ActionSUPERUSER Action = Action("superuser")
)

var ALLACTIONS = set.NewSetWith(ActionATTEMPT, ActionOPEN, ActionCLOSE, ActionADD,
	ActionCHANGE, ActionREMOVE, ActionSUPERUSER)

func (act *Action) Scan(value interface{}) error {
	*act = ""
	if value == nil {
		return logging.LogAndError("scanned NULL action (did you mean to use NullAction)")
	}
	if val, err := driver.String.ConvertValue(value); err == nil {
		valAsString := string(val.([]byte))
		*act = Action(valAsString)
		if !ALLACTIONS.Contains(*act) {
			return logging.LogAndErrorf("invalid Action %#v", *act)
		}
		return nil
	} else {
		return err
	}
}

func (act Action) Value() (driver.Value, error) {
	if !ALLACTIONS.Contains(act) {
		return nil, logging.LogAndErrorf("invalid Action %#v", act)
	}
	return []byte(string(act)), nil
}

type NullAction struct {
	Action Action
	Valid  bool
}

func (act *NullAction) Scan(value interface{}) error {
	if value == nil {
		act.Action, act.Valid = "", false
		return nil
	}
	err := act.Action.Scan(value)
	if err != nil {
		act.Valid = false
		return err
	} else {
		act.Valid = true
		return nil
	}
}

func (act NullAction) Value() (driver.Value, error) {
	if !act.Valid {
		return nil, nil
	}
	return act.Action.Value()
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*Action)(nil)
var _ driver.Valuer = ActionSUPERUSER

var _ sql.Scanner = (*NullAction)(nil)
var _ driver.Valuer = NullAction{}
