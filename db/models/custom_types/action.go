package custom_types

import (
	"database/sql"
	"database/sql/driver"

	"github.com/databrary/databrary/logging"
	set "github.com/deckarep/golang-set"
	"github.com/pkg/errors"
)

// The various activities for which we keep audit records (in audit or a derived table).
type Action string

const (
	// TODO: these should be uppercased in schema
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

// Implements Scanner interface.
// This is what is used to convert a column of type action from a postgres query
// into this Go type. The argument has the []byte representation of the column.
func (act *Action) Scan(value interface{}) error {
	if value == nil {
		err, _ := log.LogWrapErr(nil, "scanned NULL action (did you mean to use NullAction)")
		return err
	}
	if val, err := driver.String.ConvertValue(value); err == nil {
		valAsString := string(val.([]byte))
		*act = Action(valAsString)
		if !ALLACTIONS.Contains(*act) {
			err, _ := log.LogWrapErr(errors.New("invalid Action"), "%#v", *act)
			return err
		}
		return nil
	} else {
		return err
	}
}

// Implements Value interface
// This is what is used to convert a  Go type action to a postgres type.
func (act Action) Value() (driver.Value, error) {
	if !ALLACTIONS.Contains(act) {
		err, _ := log.LogWrapErr(nil, "invalid Action", "%#v", act)
		return nil, err
	}
	return []byte(string(act)), nil
}

// Nullable action. Just a wrapper around Action.
type NullAction struct {
	Action Action
	Valid  bool
}

// Implements Scanner interface.
// This is what is used to convert a column of type action from a postgres query
// into this Go type. The argument has the []byte representation of the column.
// Null scan to  act.Valid == false.
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

// Implements Valuer interface
// This is what is used to convert a  Go type action to a postgres type.
// Valid == false turns into a Null value.
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
