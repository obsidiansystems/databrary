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


type Permission string

// Levels of access parties can have to the site data.

const (
	PermissionNONE   Permission = Permission("NONE")
	PermissionPUBLIC Permission = Permission("PUBLIC")
	PermissionSHARED Permission = Permission("SHARED")
	PermissionREAD   Permission = Permission("READ")
	PermissionEDIT   Permission = Permission("EDIT")
	PermissionADMIN  Permission = Permission("ADMIN")
)

func (exp Permission) Value() (driver.Value, error) {
	// value needs to be a base driver.Value type
	// such as bool.
	return string(exp), nil
}

func (exp *Permission) Scan(value interface{}) error {
	if value == nil {
		return PermissionErrorDatabase
	}
	if exposure_val, err := driver.String.ConvertValue(value); err == nil {
		if v, ok := exposure_val.(Permission); ok {
			*exp = Permission(v)
			return nil
		}
	}
	return PermissionErrorScan
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*Permission)(nil)
var _ driver.Valuer = PermissionADMIN

type PermissionError struct {
	message string
}

func (e *PermissionError) Error() string {
	return fmt.Sprintf("%s", e.message)
}

var PermissionErrorDatabase = &PermissionError{"got nil value from database for Action for volume"}
var PermissionErrorScan = &PermissionError{"failed to scan Action"}