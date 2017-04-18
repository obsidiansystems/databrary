package models

import (
	"database/sql"

	. "github.com/databrary/databrary/db/models/custom_types/varchar"
)

type (
	// Users, groups, organizations, and other logical identities
	Party struct {
		PartyID     int64          `json:"party_id" db:"id,omitempty"`
		Name        string         `json:"party_name" db:"name"`
		PreName     sql.NullString `json:"party_prename" db:"prename"`
		ORCID       VarChar        `json:"party_orcid" db:"orcid"` // http://en.wikipedia.org/wiki/ORCID
		Affiliation sql.NullString `json:"party_affiliation" db:"affiliation"`
		Url         sql.NullString `json:"party_url" db:"url"`
	}

	// Login information for parties associated with registered individuals.
	Account struct {
		PartyID  int64   `json:"account_id" db:"id"` // references party
		Email    VarChar `json:"account_email" db:"email"`
		Password VarChar `json:"account_password" db:"password"`
	}
)
