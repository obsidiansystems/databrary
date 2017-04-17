package tests

import (
	"testing"

	"database/sql"
	"encoding/json"
	"fmt"
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db/models"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	pq "github.com/lib/pq"
	"io/ioutil"
	"reflect"
	"strings"
	"upper.io/db.v3/lib/sqlbuilder"
)

var conn sqlbuilder.Database

func createInsertTestVolume() models.Volume {
	var (
		name  = "a"
		body  = sql.NullString{"b", true}
		alias = sql.NullString{"c", true}
		doi   = sql.NullString{util.RandStringRunes(10), true}
	)
	vol := models.Volume{
		Name:  name,
		Body:  body,
		Alias: alias,
		DOI:   doi,
	}
	vols := conn.Collection("volume")
	id, err := vols.Insert(vol)
	util.CheckErr(err)
	vol.VolumeID = id.(int64)
	return vol
}

func TestVolume(t *testing.T) {
	config.InitConf("../../../config/databrary_test.toml")
	conf := config.GetConf()
	logging.InitLgr(conf)

	// create test db
	conn1 := OpenTestConn(conf, t)
	testSchemaDbName := strings.ToLower(fmt.Sprintf("testdb_%s", util.RandStringRunes(10)))
	_, err := conn1.Exec(fmt.Sprintf("CREATE DATABASE %s", testSchemaDbName))
	util.CheckErr(err)
	err = conn1.Close()

	// install schema
	conf.Set("database.db_name", testSchemaDbName)
	util.CheckErr(err)
	conn = OpenTestConn(conf, t)
	schemaFile, err := ioutil.ReadFile(conf.GetString("database.schema"))
	util.CheckErr(err)
	_, err = conn.Exec(string(schemaFile))
	util.CheckErr(err)

	// drop test db
	defer func() {
		err = conn.Close()
		util.CheckErr(err)
		// can't drop db you're connected to
		conf.Set("database.db_name", "postgres")
		conn2 := OpenTestConn(conf, t)
		_, err = conn2.Exec(fmt.Sprintf("DROP DATABASE %s", testSchemaDbName))
		util.CheckErr(err)
	}()

	t.Run("", testVolume)
	t.Run("", testVolumeAccess)
	t.Run("", testVolumeOwners)
	t.Run("", testVolumeLink)
	t.Run("", testVolumeCitatation)
	t.Run("", testFunder)
	t.Run("", testVolumeFunding)
}

func testVolume(t *testing.T) {
	vol := createInsertTestVolume()
	vols := conn.Collection("volume")
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
	name := "e"
	body := sql.NullString{"f", true}
	alias := sql.NullString{"g", true}
	doi := sql.NullString{"h", true}

	vol2 := models.Volume{
		Name:  name,
		Body:  body,
		Alias: alias,
		DOI:   doi,
	}
	id, err := vols.Insert(vol2)
	util.CheckErr(err)
	vol2.VolumeID = id.(int64)

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

func testVolumeAccess(t *testing.T) {
	vol := createInsertTestVolume()
	volA := models.VolumeAccess{
		Volume:     vol.VolumeID,
		Party:      0,
		Individual: models.PermADMIN,
		Children:   models.PermADMIN,
		Sort:       sql.NullInt64{0, true},
	}
	volAs := conn.Collection("volume_access")
	_, err := volAs.Insert(volA)
	util.CheckErr(err)
	dbVolAs := &models.VolumeAccess{}
	volAs.Find("volume = ? AND party = ?", vol.VolumeID, 0).One(dbVolAs)
	if *dbVolAs != volA {
		t.Fatal("bespoke volume access and db volume access don't match")
	}
}

func testVolumeOwners(t *testing.T) {

	vol := createInsertTestVolume()
	owners := conn.Collection("volume_owners")
	owners_test := []string{"32:Karasik, Lana", "11:Tamis-LeMonda, Catherine", "5:Adolph, Karen"}
	volOwners := models.VolumeOwners{
		Volume: vol.VolumeID,
		Owners: pq.StringArray(owners_test),
	}
	id, err := owners.Insert(volOwners)
	util.CheckErr(err)
	dbVolOwners := &models.VolumeOwners{}
	owners.Find(id).One(dbVolOwners)
	if volOwners.Volume != dbVolOwners.Volume || !reflect.DeepEqual(volOwners.Owners, dbVolOwners.Owners) {
		fmt.Println(
			volOwners.Volume == dbVolOwners.Volume,
			reflect.DeepEqual(volOwners.Owners, dbVolOwners.Owners),
		)
		util.PrintReps(volOwners, dbVolOwners)
		t.Fatal("bespoke volOwners and dbVolOwners don't match")
	}
}

func testVolumeLink(t *testing.T) {
	vol := createInsertTestVolume()
	for i := 0; i < 5; i++ {
		volL := models.VolumeLink{
			Volume: vol.VolumeID,
			Head:   util.RandStringRunes(10),
			Url:    util.RandStringRunes(10),
		}
		links := conn.Collection("volume_link")
		id, err := links.Insert(&volL)
		util.CheckErr(err)
		newVolL := models.VolumeLink{}
		links.Find(id).One(&newVolL)
		if volL != newVolL {
			util.PrintReps()
			t.Fatal("didn't match")
		}
	}
}

func testVolumeCitatation(t *testing.T) {
	vol := createInsertTestVolume()
	vc := models.VolumeCitation{
		Volume: vol.VolumeID,
		Head:   util.RandStringRunes(10),
		Url:    sql.NullString{util.RandStringRunes(10), true},
		Year:   2017,
	}
	cits := conn.Collection("volume_citation")
	id, err := cits.Insert(&vc)
	util.CheckErr(err)
	newCit := models.VolumeCitation{}
	cits.Find(id).One(&newCit)
	if vc != newCit {
		util.PrintReps(vc, newCit)
		t.Fatal()
	}
}

func testFunder(t *testing.T) {
	fun := models.Funder{
		FunderReferenceID: 12345,
		Name:              util.RandStringRunes(10),
	}
	funds := conn.Collection("funder")
	id, err := funds.Insert(&fun)
	util.CheckErr(err)
	newFund := models.Funder{}
	funds.Find(id).One(&newFund)
	if newFund != fun {
		util.PrintReps(newFund, fun)
		t.Fatal("funders don't match")
	}

}

func testVolumeFunding(t *testing.T) {
	fun := models.Funder{
		FunderReferenceID: 12346,
		Name:              util.RandStringRunes(10),
	}
	funds := conn.Collection("funder")
	id, err := funds.Insert(&fun)
	util.CheckErr(err)

	vol := createInsertTestVolume()
	volFund := conn.Collection("volume_funding")
	volFunder := models.VolumeFunding{
		Volume: vol.VolumeID,
		Funder: id.(int64),
		Awards: pq.StringArray{"a", "b", "c", "d"},
	}
	id, err = volFund.Insert(&volFunder)
	dbVolFund := models.VolumeFunding{}
	volFund.Find(id).One(&dbVolFund)
	if dbVolFund.Volume != volFunder.Volume || dbVolFund.Funder != volFunder.Funder || !reflect.DeepEqual(dbVolFund.Awards, volFunder.Awards) {
		util.PrintReps(volFunder, dbVolFund)
		t.Fatal("mismatch	")
	}

}
