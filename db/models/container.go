package models

import (
	"database/sql"

	. "github.com/databrary/databrary/db/models/custom_types"
)

type (
	// Organizational unit within volume containing related files (with common annotations),
	// often corresponding to an individual data session (single visit/acquisition/participant/group/day).
	Container struct {
		ContainerID int64          `json:"container_id" db:"id,omitempty"`
		Volume      int64          `json:"container_volume" db:"volume"` // references volume
		Top         bool           `json:"container_top" db:"top"`
		Name        sql.NullString `json:"container_name" db:"name"`
		Date        Date           `json:"container_date" db:"date"` // actually a date
	}
)

func (c Container) Equal(cc Container) bool {
	return c.ContainerID == cc.ContainerID &&
		c.Volume == cc.Volume &&
		c.Top == cc.Top &&
		c.Name == cc.Name &&
		c.Date.Time().Equal(cc.Date.Time())
}
