package tests

import (
	"fmt"
	"testing"

	"github.com/databrary/databrary/db/models"
	"github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/databrary/util"
)

func TestSlot(t *testing.T) {
	testFuncs = []testFunc{
		{"", testSlot},
	}
	test(t)
}

func createInsertSlotRelease(containerId int64) (models.SlotRelease, interface{}) {
	slots := testConn.Collection("slot_release")
	seg, _ := custom_types.NewSegment(&times[0], &times[2], "[]")
	slotRl := models.SlotRelease{
		ContainerID: containerId,
		Segment:     seg,
		Release:     custom_types.ReleaseLvlSHARED,
	}
	pkey, err := slots.Insert(slotRl)
	util.CheckErr(err)
	return slotRl, pkey
}

func testSlot(t *testing.T) {
	vol, _ := createInsertVolume()
	cont, _ := createInsertContainer(vol.VolumeID)
	dbSlotRl, pkey := createInsertSlotRelease(cont.ContainerID)
	slots := testConn.Collection("slot_release")
	newSlotRl := models.SlotRelease{}
	err := slots.Find(pkey).One(&newSlotRl)
	util.CheckErr(err)
	if dbSlotRl.Equal(&newSlotRl) {
		util.PrintReps(dbSlotRl, newSlotRl)
		fmt.Println(dbSlotRl.Segment.String())
		fmt.Println(newSlotRl.Segment.String())
		t.Fatal("mismatch")
	}
}
