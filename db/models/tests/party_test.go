package tests

import (
	"database/sql"
	"testing"

	"github.com/databrary/databrary/db/models"
	"github.com/databrary/databrary/db/models/custom_types/varchar"
	"github.com/databrary/databrary/util"
)

func TestParty(t *testing.T) {
	testFuncs = []testFunc{
		{"", testParty},
		{"", testAccount},
	}
	test(t)
}

func createInsertParty() (models.Party, interface{}) {
	party := models.Party{
		Name:        util.RandStringRunes(10),
		PreName:     sql.NullString{util.RandStringRunes(10), true},
		ORCID:       varchar.NewVarChar(util.RandStringRunes(10), true),
		Affiliation: sql.NullString{util.RandStringRunes(10), true},
		Url:         sql.NullString{util.RandStringRunes(10), true},
	}

	parties := testConn.Collection("party")
	pkey, err := parties.Insert(party)
	util.CheckErr(err)
	party.PartyID = pkey.(int64)
	return party, pkey

}

func testParty(t *testing.T) {
	dbParty, pkey := createInsertParty()
	parties := testConn.Collection("party")
	newParty := models.Party{}
	err := parties.Find(pkey).One(&newParty)
	util.CheckErr(err)
	if dbParty != newParty {
		util.PrintReps(dbParty, newParty)
		t.Fatal("mismatch")
	}

}

func createInsertAccount(partyId int64) (models.Account, interface{}) {
	accounts := testConn.Collection("account")
	account := models.Account{
		PartyID:  partyId,
		Email:    varchar.NewVarChar(util.RandStringRunes(10), true),
		Password: varchar.NewVarChar(util.RandStringRunes(10), true),
	}
	pkey, err := accounts.Insert(account)
	util.CheckErr(err)
	return account, pkey
}

func testAccount(t *testing.T) {
	accounts := testConn.Collection("account")
	newParty, _ := createInsertParty()
	dbAccount, pkey := createInsertAccount(newParty.PartyID)
	newAccount := models.Account{}
	err := accounts.Find(pkey).One(&newAccount)
	util.CheckErr(err)
	if dbAccount != newAccount {
		t.Fatal("mismatch")
	}
}
