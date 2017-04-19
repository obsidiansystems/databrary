package models

import (
	. "github.com/databrary/databrary/db/models/custom_types"
)

type (
	// Inclusions of slots (sessions) from "dataset" (provider) volumes in "study" (consumer/reuse) volumes.
	VolumeInclusion struct {
		VolumeID    int64   `json:"volume_inclusion_volume_id" db:"volume"`
		ContainerID int64   `json:"volume_inclusion_container_id" db:"container"`
		Segment     Segment `json:"volume_inclusion_segment" db:"segment"`
	}
)

func (v *VolumeInclusion) Equal(vv *VolumeInclusion) bool {
	return v.VolumeID == vv.VolumeID &&
		v.ContainerID == vv.ContainerID &&
		v.Segment.Equal(&vv.Segment)
}
