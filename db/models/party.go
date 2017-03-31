package models

import (
	"database/sql"
	"database/sql/driver"
	"fmt"
)

type (
	// Users, groups, organizations, and other logical identities
	Party struct {
		PartyID     uint           `json:"party_id" db:"id,omitempty"`
		Name        string         `json:"party_name" db:"name"`
		PreName     sql.NullString `json:"party_prename" db:"prename"`
		ORCID       sql.NullString `json:"party_orcid" db:"orcid"` // http://en.wikipedia.org/wiki/ORCID
		Affiliation sql.NullString `json:"party_affiliation" db:"affiliation"`
		Url         sql.NullString `json:"party_url" db:"url"`
		//AccountID	sql.NullInt64	`json:"account_id" db:"account_id"`
		// partyPermission :: Action -- permission current user has over this party
		// partyAccess :: Maybe Access -- direct authorization this party has granted to current user
	}

	// Login information for parties associated with registered individuals.
	Account struct {
		AccountID uint   `json:"account_id" db:"id,omitempty"`
		Email     string `json:"account_email" db:"email"`
		Password  string `json:"account_password" db:"password"`
	}
)

// Permission enum
type Perm string

// Levels of access parties can have to the site data.
const (
	PermNONE   Perm = Perm("NONE")
	PermPUBLIC Perm = Perm("PUBLIC")
	PermSHARED Perm = Perm("SHARED")
	PermREAD   Perm = Perm("READ")
	PermEDIT   Perm = Perm("EDIT")
	PermADMIN  Perm = Perm("ADMIN")
)

func (exp Perm) Value() (driver.Value, error) {
	// value needs to be a base driver.Value type
	// such as bool.
	return string(exp), nil
}

func (exp *Perm) Scan(value interface{}) error {
	if value == nil {
		return PermErrDb{
			msg: "got nil from database for Permission for volume",
		}
	}
	if exposure_val, err := driver.String.ConvertValue(value); err == nil {
		if v, ok := exposure_val.(Perm); ok {
			*exp = Perm(v)
			return nil
		}
	}
	return PermErrScn{
		msg: "failed to scan Permission",
	}
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*Perm)(nil)
var _ driver.Valuer = PermADMIN

type PermErr interface {
	error
}

type PermErrDb struct {
	msg string
}

func (e PermErrDb) Error() string {
	return fmt.Sprintf("%s", e.msg)
}

type PermErrScn struct {
	msg string
}

func (e PermErrScn) Error() string {
	return fmt.Sprint("%s", e.msg)
}

// consult volume.Test_error
