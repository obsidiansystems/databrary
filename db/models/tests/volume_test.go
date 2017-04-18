package tests

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"math/rand"
	"reflect"
	"testing"
	"time"

	"github.com/databrary/databrary/db/models"
	. "github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/databrary/util"
	"github.com/lib/pq"
)

func TestVolume(t *testing.T) {
	testFuncs = []testFunc{
		{"", testVolume},
		{"", testVolumeAccess},
		{"", testVolumeOwners},
		{"", testVolumeLink},
		{"", testVolumeCitatation},
		{"", testFunder},
		{"", testVolumeFunding},
	}
	test(t)
}

func createInsertVolume() models.Volume {
	vol := models.Volume{
		Name:  util.RandStringRunes(10),
		Body:  sql.NullString{util.RandStringRunes(10), true},
		Alias: NewVarChar(util.RandStringRunes(10), true),
		DOI:   NewVarChar(util.RandStringRunes(10), true),
	}
	vols := testConn.Collection("volume")
	id, err := vols.Insert(vol)
	util.CheckErr(err)
	vol.VolumeID = id.(int64)
	return vol
}

func testVolume(t *testing.T) {
	vol := createInsertVolume()
	vols := testConn.Collection("volume")
	res := vols.Find("id", vol.VolumeID)
	defer res.Close()
	c, err := res.Count()
	if c != 1 {
		t.Fatal("wrong count")
	}
	dbVol := &models.Volume{}
	res.Next(dbVol)
	if vol != *dbVol {
		util.PrintReps(vol, *dbVol)
		t.Fatal("volume from db and bespoke vol differ")
	}

	// fetch multiple vols
	vol2 := createInsertVolume()

	modelVols := map[int64]models.Volume{
		vol.VolumeID:  vol,
		vol2.VolumeID: vol2,
	}
	dbVols := []models.Volume{}
	err = vols.Find().All(&dbVols)
	util.CheckErr(err)
	for i, v := range dbVols {
		i := int64(i)
		if m, in := modelVols[i]; in && m != v {
			util.PrintReps(m, v)
			t.Fatal("volume from db and bespoke vol differ")
		}
	}

	jvol, _ := json.MarshalIndent(vol, "", "  ")
	fmt.Println(string(jvol))
}

func createInsertVolumeAccess(volId int64) models.VolumeAccess {

	volA := models.VolumeAccess{
		Volume:     volId,
		Party:      0,
		Individual: models.PermADMIN,
		Children:   models.PermADMIN,
		Sort:       sql.NullInt64{0, true},
	}
	volAs := testConn.Collection("volume_access")
	_, err := volAs.Insert(volA)
	util.CheckErr(err)
	return volA
}

func testVolumeAccess(t *testing.T) {
	vol := createInsertVolume()
	volA := createInsertVolumeAccess(vol.VolumeID)
	volAs := testConn.Collection("volume_access")
	dbVolAs := &models.VolumeAccess{}
	volAs.Find("volume = ? AND party = ?", vol.VolumeID, 0).One(dbVolAs)
	if *dbVolAs != volA {
		t.Fatal("bespoke volume access and db volume access don't match")
	}
}

func createInsertVolumeOwners(volId int64, ownersStr []string) (models.VolumeOwners, interface{}) {
	owners := testConn.Collection("volume_owners")
	volOwners := models.VolumeOwners{
		Volume: volId,
		Owners: pq.StringArray(ownersStr),
	}
	pkey, err := owners.Insert(volOwners)
	util.CheckErr(err)
	return volOwners, pkey
}

func testVolumeOwners(t *testing.T) {

	vol := createInsertVolume()
	owners := testConn.Collection("volume_owners")
	owners_test := []string{"32:Karasik, Lana", "11:Tamis-LeMonda, Catherine", "5:Adolph, Karen"}
	volOwners, pkey := createInsertVolumeOwners(vol.VolumeID, owners_test)
	dbVolOwners := &models.VolumeOwners{}
	owners.Find(pkey).One(dbVolOwners)
	if volOwners.Volume != dbVolOwners.Volume || !reflect.DeepEqual(volOwners.Owners, dbVolOwners.Owners) {
		fmt.Println(
			volOwners.Volume == dbVolOwners.Volume,
			reflect.DeepEqual(volOwners.Owners, dbVolOwners.Owners),
		)
		util.PrintReps(volOwners, dbVolOwners)
		t.Fatal("bespoke volOwners and dbVolOwners don't match")
	}
}

func createInsertVolumeLink(volId int64) (models.VolumeLink, interface{}) {
	volL := models.VolumeLink{
		Volume: volId,
		Head:   util.RandStringRunes(10),
		Url:    util.RandStringRunes(10),
	}
	links := testConn.Collection("volume_link")
	pkey, err := links.Insert(&volL)
	util.CheckErr(err)
	return volL, pkey
}

func testVolumeLink(t *testing.T) {
	for i := 0; i < 5; i++ {
		links := testConn.Collection("volume_link")
		vol := createInsertVolume()
		volL, pkey := createInsertVolumeLink(vol.VolumeID)
		newVolL := models.VolumeLink{}
		links.Find(pkey).One(&newVolL)
		if volL != newVolL {
			util.PrintReps()
			t.Fatal("didn't match")
		}
	}
}

func createInsertVolumeCitation(volId int64) (models.VolumeCitation, interface{}) {
	vc := models.VolumeCitation{
		Volume: volId,
		Head:   util.RandStringRunes(10),
		Url:    sql.NullString{util.RandStringRunes(10), true},
		Year:   2017,
	}
	cits := testConn.Collection("volume_citation")
	pkey, err := cits.Insert(&vc)
	util.CheckErr(err)
	return vc, pkey
}

func testVolumeCitatation(t *testing.T) {
	vol := createInsertVolume()
	volC, pkey := createInsertVolumeCitation(vol.VolumeID)
	cits := testConn.Collection("volume_citation")

	newCit := models.VolumeCitation{}
	cits.Find(pkey).One(&newCit)
	if volC != newCit {
		util.PrintReps(volC, newCit)
		t.Fatal()
	}
}

func createInsertFunder() (models.Funder, interface{}) {
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	fun := models.Funder{
		FunderReferenceID: int64(r.Int31()),
		Name:              util.RandStringRunes(10),
	}
	funds := testConn.Collection("funder")
	pkey, err := funds.Insert(&fun)
	util.CheckErr(err)
	return fun, pkey
}

func testFunder(t *testing.T) {
	funds := testConn.Collection("funder")
	fun, pkey := createInsertFunder()
	newFund := models.Funder{}
	funds.Find(pkey).One(&newFund)
	if newFund != fun {
		util.PrintReps(newFund, fun)
		t.Fatal("funders don't match")
	}

}

func createInsertVolumeFunding(funderId, volId int64) (models.VolumeFunding, interface{}) {
	volFund := testConn.Collection("volume_funding")
	volFunder := models.VolumeFunding{
		Volume: volId,
		Funder: funderId,
		Awards: pq.StringArray{
			util.RandStringRunes(10),
			util.RandStringRunes(10),
			util.RandStringRunes(10),
		},
	}
	pkey, err := volFund.Insert(&volFunder)
	util.CheckErr(err)
	return volFunder, pkey
}

func testVolumeFunding(t *testing.T) {
	fun, _ := createInsertFunder()
	vol := createInsertVolume()
	volFund := testConn.Collection("volume_funding")
	volFunder, pkey := createInsertVolumeFunding(fun.FunderReferenceID, vol.VolumeID)
	dbVolFund := models.VolumeFunding{}
	volFund.Find(pkey).One(&dbVolFund)
	if dbVolFund.Volume != volFunder.Volume || dbVolFund.Funder != volFunder.Funder || !reflect.DeepEqual(dbVolFund.Awards, volFunder.Awards) {
		util.PrintReps(volFunder, dbVolFund)
		t.Fatal("mismatch	")
	}

}
