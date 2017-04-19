package tests

import (
	"database/sql"
	"testing"

	"github.com/databrary/databrary/db"
	"github.com/databrary/databrary/db/models"
	. "github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/databrary/util"
)

func TestContainer(t *testing.T) {
	testFuncs = []testFunc{
		{"", testContainer},
	}
	test(t)
}

func createInsertContainer(volId int64) (models.Container, interface{}) {
	conts := testConn.Collection("container")
	cont := models.Container{
		Volume: volId,
		Top:    true,
		Name:   sql.NullString{util.RandStringRunes(10), true},
		//Date: NewDate(util.Now(), true),
		Date: NewDate(db.PgToday(), true),
	}
	pkey, err := conts.Insert(cont)
	cont.ContainerID = pkey.(int64)
	util.CheckErr(err)
	return cont, pkey
}

func testContainer(t *testing.T) {
	vol, _ := createInsertVolume()
	dbCont, pkey := createInsertContainer(vol.VolumeID)
	newCont := models.Container{}
	conts := testConn.Collection("container")
	err := conts.Find(pkey).One(&newCont)
	util.CheckErr(err)
	if !dbCont.Equal(newCont) {
		util.PrintReps(dbCont, newCont)
		t.Fatal("mismatch")
	}

}
