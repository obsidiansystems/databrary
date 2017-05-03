// This file is generated by SQLBoiler (https://github.com/vattle/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// EDIT AT YOUR OWN RISK

package models

import (
	"bytes"
	"os"
	"os/exec"
	"reflect"
	"sort"
	"strings"
	"testing"

	"github.com/databrary/databrary/db/models/custom_types"
	"github.com/pmezard/go-difflib/difflib"
	"github.com/vattle/sqlboiler/boil"
	"github.com/vattle/sqlboiler/randomize"
	"github.com/vattle/sqlboiler/strmangle"
)

func testVolumeAccesses(t *testing.T) {
	t.Parallel()

	query := VolumeAccesses(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}

func testVolumeAccessesLive(t *testing.T) {
	all, err := VolumeAccesses(dbMain.liveDbConn).All()
	if err != nil {
		t.Fatalf("failed to get all VolumeAccesses err: ", err)
	}
	tx, err := dbMain.liveTestDbConn.Begin()
	if err != nil {
		t.Fatalf("failed to begin transaction: ", err)
	}
	for _, v := range all {
		err := v.Insert(tx)
		if err != nil {
			t.Fatalf("failed to failed to insert %s because of %s", v, err)
		}

	}
	err = tx.Commit()
	if err != nil {
		t.Fatalf("failed to commit transaction: ", err)
	}
	bf := &bytes.Buffer{}
	dumpCmd := exec.Command("psql", `-c "COPY (SELECT * FROM volume_access) TO STDOUT" -d `, dbMain.DbName)
	dumpCmd.Env = append(os.Environ(), dbMain.pgEnv()...)
	dumpCmd.Stdout = bf
	err = dumpCmd.Start()
	if err != nil {
		t.Fatalf("failed to start dump from live db because of %s", err)
	}
	dumpCmd.Wait()
	if err != nil {
		t.Fatalf("failed to wait dump from live db because of %s", err)
	}
	bg := &bytes.Buffer{}
	dumpCmd = exec.Command("psql", `-c "COPY (SELECT * FROM volume_access) TO STDOUT" -d `, dbMain.LiveTestDBName)
	dumpCmd.Env = append(os.Environ(), dbMain.pgEnv()...)
	dumpCmd.Stdout = bg
	err = dumpCmd.Start()
	if err != nil {
		t.Fatalf("failed to start dump from test db because of %s", err)
	}
	dumpCmd.Wait()
	if err != nil {
		t.Fatalf("failed to wait dump from test db because of %s", err)
	}
	bfslice := sort.StringSlice(difflib.SplitLines(bf.String()))
	gfslice := sort.StringSlice(difflib.SplitLines(bg.String()))
	bfslice.Sort()
	gfslice.Sort()
	diff := difflib.ContextDiff{
		A:        bfslice,
		B:        gfslice,
		FromFile: "databrary",
		ToFile:   "test",
		Context:  1,
	}
	result, _ := difflib.GetContextDiffString(diff)
	if len(result) > 0 {
		t.Fatalf("VolumeAccessesLive failed but it's probably trivial: %s", strings.Replace(result, "\t", " ", -1))
	}

}

func testVolumeAccessesDelete(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = volumeAccess.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeAccessesQueryDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = VolumeAccesses(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeAccessesSliceDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := VolumeAccessSlice{volumeAccess}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}
func testVolumeAccessesExists(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := VolumeAccessExists(tx, volumeAccess.Volume, volumeAccess.Party)
	if err != nil {
		t.Errorf("Unable to check if VolumeAccess exists: %s", err)
	}
	if !e {
		t.Errorf("Expected VolumeAccessExistsG to return true, but got false.")
	}
}
func testVolumeAccessesFind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	volumeAccessFound, err := FindVolumeAccess(tx, volumeAccess.Volume, volumeAccess.Party)
	if err != nil {
		t.Error(err)
	}

	if volumeAccessFound == nil {
		t.Error("want a record, got nil")
	}
}
func testVolumeAccessesBind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = VolumeAccesses(tx).Bind(volumeAccess); err != nil {
		t.Error(err)
	}
}

func testVolumeAccessesOne(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := VolumeAccesses(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testVolumeAccessesAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccessOne := &VolumeAccess{}
	volumeAccessTwo := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccessOne, volumeAccessDBTypes, false, volumeAccessColumnsWithCustom...); err != nil {

		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}
	if err = randomize.Struct(seed, volumeAccessTwo, volumeAccessDBTypes, false, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccessOne.Individual = custom_types.PermissionRandom()
	volumeAccessTwo.Individual = custom_types.PermissionRandom()
	volumeAccessOne.Children = custom_types.PermissionRandom()
	volumeAccessTwo.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccessOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = volumeAccessTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := VolumeAccesses(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testVolumeAccessesCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccessOne := &VolumeAccess{}
	volumeAccessTwo := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccessOne, volumeAccessDBTypes, false, volumeAccessColumnsWithCustom...); err != nil {

		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}
	if err = randomize.Struct(seed, volumeAccessTwo, volumeAccessDBTypes, false, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccessOne.Individual = custom_types.PermissionRandom()
	volumeAccessTwo.Individual = custom_types.PermissionRandom()
	volumeAccessOne.Children = custom_types.PermissionRandom()
	volumeAccessTwo.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccessOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = volumeAccessTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}

func volumeAccessBeforeInsertHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func volumeAccessAfterInsertHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func volumeAccessAfterSelectHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func volumeAccessBeforeUpdateHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func volumeAccessAfterUpdateHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func volumeAccessBeforeDeleteHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func volumeAccessAfterDeleteHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func volumeAccessBeforeUpsertHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func volumeAccessAfterUpsertHook(e boil.Executor, o *VolumeAccess) error {
	*o = VolumeAccess{}
	return nil
}

func testVolumeAccessesHooks(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	empty := &VolumeAccess{}

	AddVolumeAccessHook(boil.BeforeInsertHook, volumeAccessBeforeInsertHook)
	if err = volumeAccess.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessBeforeInsertHooks = []VolumeAccessHook{}

	AddVolumeAccessHook(boil.AfterInsertHook, volumeAccessAfterInsertHook)
	if err = volumeAccess.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessAfterInsertHooks = []VolumeAccessHook{}

	AddVolumeAccessHook(boil.AfterSelectHook, volumeAccessAfterSelectHook)
	if err = volumeAccess.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessAfterSelectHooks = []VolumeAccessHook{}

	AddVolumeAccessHook(boil.BeforeUpdateHook, volumeAccessBeforeUpdateHook)
	if err = volumeAccess.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessBeforeUpdateHooks = []VolumeAccessHook{}

	AddVolumeAccessHook(boil.AfterUpdateHook, volumeAccessAfterUpdateHook)
	if err = volumeAccess.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessAfterUpdateHooks = []VolumeAccessHook{}

	AddVolumeAccessHook(boil.BeforeDeleteHook, volumeAccessBeforeDeleteHook)
	if err = volumeAccess.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessBeforeDeleteHooks = []VolumeAccessHook{}

	AddVolumeAccessHook(boil.AfterDeleteHook, volumeAccessAfterDeleteHook)
	if err = volumeAccess.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessAfterDeleteHooks = []VolumeAccessHook{}

	AddVolumeAccessHook(boil.BeforeUpsertHook, volumeAccessBeforeUpsertHook)
	if err = volumeAccess.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessBeforeUpsertHooks = []VolumeAccessHook{}

	AddVolumeAccessHook(boil.AfterUpsertHook, volumeAccessAfterUpsertHook)
	if err = volumeAccess.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeAccess, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", volumeAccess)
	}
	volumeAccessAfterUpsertHooks = []VolumeAccessHook{}
}
func testVolumeAccessesInsert(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testVolumeAccessesInsertWhitelist(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx, volumeAccessColumns...); err != nil {
		t.Error(err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testVolumeAccessToOnePartyUsingParty(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var foreign Party
	var local VolumeAccess

	foreignBlacklist := partyColumnsWithDefault
	if err := randomize.Struct(seed, &foreign, partyDBTypes, true, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Party struct: %s", err)
	}
	localBlacklist := volumeAccessColumnsWithDefault
	localBlacklist = append(localBlacklist, volumeAccessColumnsWithCustom...)

	if err := randomize.Struct(seed, &local, volumeAccessDBTypes, true, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}
	local.Individual = custom_types.PermissionRandom()
	local.Children = custom_types.PermissionRandom()

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Party = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.PartyByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := VolumeAccessSlice{&local}
	if err = local.L.LoadParty(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Party == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Party = nil
	if err = local.L.LoadParty(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Party == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testVolumeAccessToOneVolumeUsingVolume(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var foreign Volume
	var local VolumeAccess

	foreignBlacklist := volumeColumnsWithDefault
	if err := randomize.Struct(seed, &foreign, volumeDBTypes, true, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Volume struct: %s", err)
	}
	localBlacklist := volumeAccessColumnsWithDefault
	localBlacklist = append(localBlacklist, volumeAccessColumnsWithCustom...)

	if err := randomize.Struct(seed, &local, volumeAccessDBTypes, true, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}
	local.Individual = custom_types.PermissionRandom()
	local.Children = custom_types.PermissionRandom()

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Volume = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.VolumeByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := VolumeAccessSlice{&local}
	if err = local.L.LoadVolume(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Volume == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Volume = nil
	if err = local.L.LoadVolume(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Volume == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testVolumeAccessToOneSetOpPartyUsingParty(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a VolumeAccess
	var b, c Party

	foreignBlacklist := strmangle.SetComplement(partyPrimaryKeyColumns, partyColumnsWithoutDefault)
	if err := randomize.Struct(seed, &b, partyDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Party struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, partyDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Party struct: %s", err)
	}
	localBlacklist := strmangle.SetComplement(volumeAccessPrimaryKeyColumns, volumeAccessColumnsWithoutDefault)
	localBlacklist = append(localBlacklist, volumeAccessColumnsWithCustom...)

	if err := randomize.Struct(seed, &a, volumeAccessDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}
	a.Individual = custom_types.PermissionRandom()
	a.Children = custom_types.PermissionRandom()

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Party{&b, &c} {
		err = a.SetParty(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Party != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.VolumeAccesses[0] != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Party != x.ID {
			t.Error("foreign key was wrong value", a.Party)
		}

		if exists, err := VolumeAccessExists(tx, a.Volume, a.Party); err != nil {
			t.Fatal(err)
		} else if !exists {
			t.Error("want 'a' to exist")
		}

	}
}
func testVolumeAccessToOneSetOpVolumeUsingVolume(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a VolumeAccess
	var b, c Volume

	foreignBlacklist := strmangle.SetComplement(volumePrimaryKeyColumns, volumeColumnsWithoutDefault)
	if err := randomize.Struct(seed, &b, volumeDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Volume struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, volumeDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Volume struct: %s", err)
	}
	localBlacklist := strmangle.SetComplement(volumeAccessPrimaryKeyColumns, volumeAccessColumnsWithoutDefault)
	localBlacklist = append(localBlacklist, volumeAccessColumnsWithCustom...)

	if err := randomize.Struct(seed, &a, volumeAccessDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}
	a.Individual = custom_types.PermissionRandom()
	a.Children = custom_types.PermissionRandom()

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Volume{&b, &c} {
		err = a.SetVolume(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Volume != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.VolumeAccesses[0] != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Volume != x.ID {
			t.Error("foreign key was wrong value", a.Volume)
		}

		if exists, err := VolumeAccessExists(tx, a.Volume, a.Party); err != nil {
			t.Fatal(err)
		} else if !exists {
			t.Error("want 'a' to exist")
		}

	}
}
func testVolumeAccessesReload(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = volumeAccess.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeAccessesReloadAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := VolumeAccessSlice{volumeAccess}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}
func testVolumeAccessesSelect(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := VolumeAccesses(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	volumeAccessDBTypes = map[string]string{`Children`: `enum.permission('NONE','PUBLIC','SHARED','READ','EDIT','ADMIN')`, `Individual`: `enum.permission('NONE','PUBLIC','SHARED','READ','EDIT','ADMIN')`, `Party`: `integer`, `Sort`: `smallint`, `Volume`: `integer`}
	_                   = bytes.MinRead
)

func testVolumeAccessesUpdate(t *testing.T) {
	t.Parallel()

	if len(volumeAccessColumns) == len(volumeAccessPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := volumeAccessColumnsWithDefault
	blacklist = append(blacklist, volumeAccessColumnsWithCustom...)

	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	if err = volumeAccess.Update(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeAccessesSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(volumeAccessColumns) == len(volumeAccessPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := volumeAccessPrimaryKeyColumns
	blacklist = append(blacklist, volumeAccessColumnsWithCustom...)

	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(volumeAccessColumns, volumeAccessPrimaryKeyColumns) {
		fields = volumeAccessColumns
	} else {
		fields = strmangle.SetComplement(
			volumeAccessColumns,
			volumeAccessPrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(volumeAccess))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := VolumeAccessSlice{volumeAccess}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}
func testVolumeAccessesUpsert(t *testing.T) {
	t.Parallel()

	if len(volumeAccessColumns) == len(volumeAccessPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	volumeAccess := &VolumeAccess{}
	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, true, volumeAccessColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeAccess.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert VolumeAccess: %s", err)
	}

	count, err := VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	blacklist := volumeAccessPrimaryKeyColumns

	blacklist = append(blacklist, volumeAccessColumnsWithCustom...)

	if err = randomize.Struct(seed, volumeAccess, volumeAccessDBTypes, false, blacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeAccess struct: %s", err)
	}

	volumeAccess.Individual = custom_types.PermissionRandom()
	volumeAccess.Children = custom_types.PermissionRandom()

	if err = volumeAccess.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert VolumeAccess: %s", err)
	}

	count, err = VolumeAccesses(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
