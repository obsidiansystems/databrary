package models

import "time"

type (
	// Relationships and permissions granted between parties.
	Authorize struct {
		Child   int64     `json:"audit_child" db:"child"`    // Party granted permissions
		Parent  int64     `json:"audit_parent" db:"parent"`  // Party granting permissions
		Site    Perm      `json:"audit_site" db:"site"`      // Level of site access granted to child, inherited (but degraded) from parent
		Member  Perm      `json:"audit_memeber" db:"member"` // Level of permission granted to the child as a member of the parent's group
		Expires time.Time `json:"audit_expires" db:"expires"`
	}
)
