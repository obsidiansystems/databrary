package models

import (
	"database/sql"
	"database/sql/driver"
	"github.com/databrary/databrary/logging"
)

type (
	// Users, groups, organizations, and other logical identities
	Party struct {
		PartyID     int64          `json:"party_id" db:"id,omitempty"`
		Name        string         `json:"party_name" db:"name"`
		PreName     sql.NullString `json:"party_prename" db:"prename"`
		ORCID       sql.NullString `json:"party_orcid" db:"orcid"` // http://en.wikipedia.org/wiki/ORCID
		Affiliation sql.NullString `json:"party_affiliation" db:"affiliation"`
		Url         sql.NullString `json:"party_url" db:"url"`
	}

	// Login information for parties associated with registered individuals.
	Account struct {
		AccountID int64  `json:"account_id" db:"id,omitempty"`
		Email     string `json:"account_email" db:"email"`
		Password  string `json:"account_password" db:"password"`
	}
)

// Permission enum
type Perm sql.NullString

// Levels of access parties can have to the site data.
var (
	PermNONE   Perm = Perm{"NONE", true}
	PermPUBLIC Perm = Perm{"PUBLIC", true}
	PermSHARED Perm = Perm{"SHARED", true}
	PermREAD   Perm = Perm{"READ", true}
	PermEDIT   Perm = Perm{"EDIT", true}
	PermADMIN  Perm = Perm{"ADMIN", true}
)

func (p Perm) Value() (driver.Value, error) {
	// value needs to be a base driver.Value type
	// such as bool.
	if p.Valid {
		return p.String, nil
	} else {
		return nil, nil
	}
}

func (p *Perm) Scan(value interface{}) error {
	if value == nil {
		return nil
	}
	if val, err := driver.String.ConvertValue(value); err == nil {
		if val, ok := val.([]byte); ok {
			*p = Perm{string(val), true}
			return nil
		}
	}
	return logging.LogAndError("failed to scan Permission")
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*Perm)(nil)
var _ driver.Valuer = PermADMIN
