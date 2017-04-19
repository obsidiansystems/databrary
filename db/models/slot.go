package models

import (
	. "github.com/databrary/databrary/db/models/custom_types"
)

type (
	// Sharing/release permissions granted by participants on (portions of) contained data.
	SlotRelease struct {
		ContainerID int64      `json:"slot_release_container_id" db:"container"`
		Segment     Segment    `json:"slot_release_segment" db:"segment"`
		Release     ReleaseLvl `json:"slot_release_release_lvl" db:"release"`
	}
)

func (s *SlotRelease) Equal(ss *SlotRelease) bool {
	return s.ContainerID == ss.ContainerID &&
		s.Release == ss.Release &&
		s.Segment.Equal(&ss.Segment)
}
