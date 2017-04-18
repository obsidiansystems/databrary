package models

import (
	"database/sql"
	"database/sql/driver"
	"github.com/databrary/databrary/logging"
	"time"
)

type (
	// Relationships and permissions granted between parties.
	Authorize struct {
		ChildParty  int64     `json:"auth_child_party_id" db:"child"`   // Party granted permissions
		ParentParty int64     `json:"auth_parent_party_id" db:"parent"` // Party granting permissions
		SitePerm    Perm      `json:"auth_site_perm" db:"site"`         // Level of site access granted to child, inherited (but degraded) from parent
		MemberPerm  Perm      `json:"auth_memeber_perm" db:"member"`    // Level of permission granted to the child as a member of the parent's group
		Expires     time.Time `json:"auth_expires" db:"expires"`
	}
)

func (a Authorize) Equal(aa Authorize) bool {
	return a.Expires.Equal(aa.Expires) &&
		a.MemberPerm == aa.MemberPerm &&
		a.SitePerm == aa.SitePerm &&
		a.ParentParty == aa.ParentParty &&
		a.ChildParty == aa.ChildParty
}

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
