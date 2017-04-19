package models

import (
	. "github.com/databrary/databrary/db/models/custom_types"
	"github.com/lib/pq"
)

type (
	// Relationships and permissions granted between parties.
	Authorize struct {
		ChildParty  int64       `json:"auth_child_party_id" db:"child"`   // Party granted permissions
		ParentParty int64       `json:"auth_parent_party_id" db:"parent"` // Party granting permissions
		SitePerm    Permission  `json:"auth_site_perm" db:"site"`         // Level of site access granted to child, inherited (but degraded) from parent
		MemberPerm  Permission  `json:"auth_memeber_perm" db:"member"`    // Level of permission granted to the child as a member of the parent's group
		Expires     pq.NullTime `json:"auth_expires" db:"expires"`
	}
)

func (a Authorize) Equal(aa Authorize) bool {
	return a.Expires.Time.Equal(aa.Expires.Time) &&
		a.MemberPerm == aa.MemberPerm &&
		a.SitePerm == aa.SitePerm &&
		a.ParentParty == aa.ParentParty &&
		a.ChildParty == aa.ChildParty
}
