package tests

import (
	"fmt"
	"testing"

	"github.com/databrary/databrary/db/models"
	"github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/databrary/util"
)

func TestStudies(t *testing.T) {
	testFuncs = []testFunc{
		{"", testVolumeInclusion},
	}
	test(t)
}

func createInsertVolumeInclusion(volumeId, containerId int64) (models.VolumeInclusion, interface{}) {
	volIs := testConn.Collection("volume_inclusion")
	seg, _ := custom_types.NewSegment(&times[0], &times[2], "[]")
	volI := models.VolumeInclusion{
		VolumeID:    volumeId,
		ContainerID: containerId,
		Segment:     seg,
	}
	pkey, err := volIs.Insert(volI)
	util.CheckErr(err)
	return volI, pkey
}

func testVolumeInclusion(t *testing.T) {
	vol, _ := createInsertVolume()
	cont, _ := createInsertContainer(vol.VolumeID)
	dbVolI, pkey := createInsertVolumeInclusion(vol.VolumeID, cont.ContainerID)
	volIs := testConn.Collection("volume_inclusion")
	newVolI := models.VolumeInclusion{}
	err := volIs.Find(pkey).One(&newVolI)
	util.CheckErr(err)
	if dbVolI.Equal(&newVolI) {
		util.PrintReps(dbVolI, newVolI)
		fmt.Println(dbVolI.Segment.String())
		fmt.Println(newVolI.Segment.String())
		t.Fatal("mismatch")
	}
}
