package tests

import (
	"testing"

	"github.com/databrary/databrary/db/models"
	"github.com/databrary/databrary/util"
	"github.com/lib/pq"
)

func TestAuthorize(t *testing.T) {
	testFuncs = []testFunc{
		{"", testAuthorize},
	}
	test(t)
}

func createInsertAuthorize(childPartyId, parentPartyId int64) (models.Authorize, interface{}) {
	auth := models.Authorize{
		ChildParty:  childPartyId,
		ParentParty: parentPartyId,
		SitePerm:    models.PermADMIN,
		MemberPerm:  models.PermADMIN,
		// only use util.Now() because it rounds to microsecond (which is the highest precision postgres uses)
		Expires: pq.NullTime{util.Now(), true},
	}
	auths := testConn.Collection("authorize")
	pkey, err := auths.Insert(auth)
	util.CheckErr(err)
	return auth, pkey
}

func testAuthorize(t *testing.T) {
	child, _ := createInsertParty()
	parent, _ := createInsertParty()
	dbAuth, pkey := createInsertAuthorize(child.PartyID, parent.PartyID)
	auths := testConn.Collection("authorize")
	newAuth := models.Authorize{}
	err := auths.Find(pkey).One(&newAuth)
	util.CheckErr(err)
	if !dbAuth.Equal(newAuth) {
		util.PrintReps(dbAuth, newAuth)
		t.Fatal("mismatch")
	}
}
