package models

import (
	"time"

	. "github.com/databrary/databrary/db/models/custom_types"
)

type (
	// Logs of all activities on the site, including access and modifications to any data.
	// Each table has an associated audit table inheriting from this one.
	Audit struct {
		Time      time.Time `json:"audit_time" db:"audit_time"`
		UserID    int64     `json:"audit_user" db:"audit_user"` // references party
		IpAddress Inet      `json:"audit_ip" db:"audit_ip"`
		Action    Action    `json:"audit_action" db:"audit_action"`
	}
)
