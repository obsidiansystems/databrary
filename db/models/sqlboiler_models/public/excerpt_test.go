// This file is generated by SQLBoiler (https://github.com/databrary/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// EDIT AT YOUR OWN RISK

package public

import (
	"bytes"
	"github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/sqlboiler/boil"
	"github.com/databrary/sqlboiler/randomize"
	"github.com/databrary/sqlboiler/strmangle"
	"github.com/pmezard/go-difflib/difflib"
	"os"
	"os/exec"
	"reflect"
	"sort"
	"strings"
	"testing"
)

func testExcerpts(t *testing.T) {
	t.Parallel()

	query := Excerpts(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}

func testExcerptsLive(t *testing.T) {
	all, err := Excerpts(dbMain.liveDbConn).All()
	if err != nil {
		t.Fatalf("failed to get all Excerpts err: ", err)
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
	dumpCmd := exec.Command("psql", `-c "COPY (SELECT * FROM excerpt) TO STDOUT" -d `, dbMain.DbName)
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
	dumpCmd = exec.Command("psql", `-c "COPY (SELECT * FROM excerpt) TO STDOUT" -d `, dbMain.LiveTestDBName)
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
		t.Fatalf("ExcerptsLive failed but it's probably trivial: %s", strings.Replace(result, "\t", " ", -1))
	}

}

func testExcerptsDelete(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = excerpt.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testExcerptsQueryDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = Excerpts(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testExcerptsSliceDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := ExcerptSlice{excerpt}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testExcerptsExists(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := ExcerptExists(tx, excerpt.Asset, excerpt.Segment)
	if err != nil {
		t.Errorf("Unable to check if Excerpt exists: %s", err)
	}
	if !e {
		t.Errorf("Expected ExcerptExistsG to return true, but got false.")
	}
}

func testExcerptsFind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	excerptFound, err := FindExcerpt(tx, excerpt.Asset, excerpt.Segment)
	if err != nil {
		t.Error(err)
	}

	if excerptFound == nil {
		t.Error("want a record, got nil")
	}
}

func testExcerptsBind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = Excerpts(tx).Bind(excerpt); err != nil {
		t.Error(err)
	}
}

func testExcerptsOne(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := Excerpts(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testExcerptsAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerptOne := &Excerpt{}
	excerptTwo := &Excerpt{}
	if err = randomize.Struct(seed, excerptOne, excerptDBTypes, false, excerptColumnsWithCustom...); err != nil {

		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}
	if err = randomize.Struct(seed, excerptTwo, excerptDBTypes, false, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerptOne.Segment = custom_types.SegmentRandom()
	excerptTwo.Segment = custom_types.SegmentRandom()
	excerptOne.Release = custom_types.NullReleaseRandom()
	excerptTwo.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerptOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = excerptTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := Excerpts(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testExcerptsCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerptOne := &Excerpt{}
	excerptTwo := &Excerpt{}
	if err = randomize.Struct(seed, excerptOne, excerptDBTypes, false, excerptColumnsWithCustom...); err != nil {

		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}
	if err = randomize.Struct(seed, excerptTwo, excerptDBTypes, false, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerptOne.Segment = custom_types.SegmentRandom()
	excerptTwo.Segment = custom_types.SegmentRandom()
	excerptOne.Release = custom_types.NullReleaseRandom()
	excerptTwo.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerptOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = excerptTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}

func excerptBeforeInsertHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func excerptAfterInsertHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func excerptAfterSelectHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func excerptBeforeUpdateHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func excerptAfterUpdateHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func excerptBeforeDeleteHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func excerptAfterDeleteHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func excerptBeforeUpsertHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func excerptAfterUpsertHook(e boil.Executor, o *Excerpt) error {
	*o = Excerpt{}
	return nil
}

func testExcerptsHooks(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	empty := &Excerpt{}

	AddExcerptHook(boil.BeforeInsertHook, excerptBeforeInsertHook)
	if err = excerpt.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", excerpt)
	}
	excerptBeforeInsertHooks = []ExcerptHook{}

	AddExcerptHook(boil.AfterInsertHook, excerptAfterInsertHook)
	if err = excerpt.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", excerpt)
	}
	excerptAfterInsertHooks = []ExcerptHook{}

	AddExcerptHook(boil.AfterSelectHook, excerptAfterSelectHook)
	if err = excerpt.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", excerpt)
	}
	excerptAfterSelectHooks = []ExcerptHook{}

	AddExcerptHook(boil.BeforeUpdateHook, excerptBeforeUpdateHook)
	if err = excerpt.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", excerpt)
	}
	excerptBeforeUpdateHooks = []ExcerptHook{}

	AddExcerptHook(boil.AfterUpdateHook, excerptAfterUpdateHook)
	if err = excerpt.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", excerpt)
	}
	excerptAfterUpdateHooks = []ExcerptHook{}

	AddExcerptHook(boil.BeforeDeleteHook, excerptBeforeDeleteHook)
	if err = excerpt.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", excerpt)
	}
	excerptBeforeDeleteHooks = []ExcerptHook{}

	AddExcerptHook(boil.AfterDeleteHook, excerptAfterDeleteHook)
	if err = excerpt.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", excerpt)
	}
	excerptAfterDeleteHooks = []ExcerptHook{}

	AddExcerptHook(boil.BeforeUpsertHook, excerptBeforeUpsertHook)
	if err = excerpt.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", excerpt)
	}
	excerptBeforeUpsertHooks = []ExcerptHook{}

	AddExcerptHook(boil.AfterUpsertHook, excerptAfterUpsertHook)
	if err = excerpt.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(excerpt, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", excerpt)
	}
	excerptAfterUpsertHooks = []ExcerptHook{}
}
func testExcerptsInsert(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testExcerptsInsertWhitelist(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx, excerptColumns...); err != nil {
		t.Error(err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testExcerptToOneSlotAssetUsingAsset(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var foreign SlotAsset
	var local Excerpt

	foreignBlacklist := slotAssetColumnsWithDefault
	foreignBlacklist = append(foreignBlacklist, slotAssetColumnsWithCustom...)

	if err := randomize.Struct(seed, &foreign, slotAssetDBTypes, true, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize SlotAsset struct: %s", err)
	}
	foreign.Segment = custom_types.SegmentRandom()

	localBlacklist := excerptColumnsWithDefault
	localBlacklist = append(localBlacklist, excerptColumnsWithCustom...)

	if err := randomize.Struct(seed, &local, excerptDBTypes, true, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}
	local.Segment = custom_types.SegmentRandom()
	local.Release = custom_types.NullReleaseRandom()

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Asset = foreign.Asset
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.AssetByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.Asset != foreign.Asset {
		t.Errorf("want: %v, got %v", foreign.Asset, check.Asset)
	}

	slice := ExcerptSlice{&local}
	if err = local.L.LoadAsset(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Asset == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Asset = nil
	if err = local.L.LoadAsset(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Asset == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testExcerptToOneSetOpSlotAssetUsingAsset(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a Excerpt
	var b, c SlotAsset

	foreignBlacklist := strmangle.SetComplement(slotAssetPrimaryKeyColumns, slotAssetColumnsWithoutDefault)
	foreignBlacklist = append(foreignBlacklist, slotAssetColumnsWithCustom...)

	if err := randomize.Struct(seed, &b, slotAssetDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize SlotAsset struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, slotAssetDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize SlotAsset struct: %s", err)
	}
	b.Segment = custom_types.SegmentRandom()
	c.Segment = custom_types.SegmentRandom()

	localBlacklist := strmangle.SetComplement(excerptPrimaryKeyColumns, excerptColumnsWithoutDefault)
	localBlacklist = append(localBlacklist, excerptColumnsWithCustom...)

	if err := randomize.Struct(seed, &a, excerptDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}
	a.Segment = custom_types.SegmentRandom()
	a.Release = custom_types.NullReleaseRandom()

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*SlotAsset{&b, &c} {
		err = a.SetAsset(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Asset != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.AssetExcerpts[0] != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Asset != x.Asset {
			t.Error("foreign key was wrong value", a.Asset)
		}

		if exists, err := ExcerptExists(tx, a.Asset, a.Segment); err != nil {
			t.Fatal(err)
		} else if !exists {
			t.Error("want 'a' to exist")
		}

	}
}

func testExcerptsReload(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = excerpt.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testExcerptsReloadAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := ExcerptSlice{excerpt}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}

func testExcerptsSelect(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := Excerpts(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	excerptDBTypes = map[string]string{`Asset`: `integer`, `Release`: `enum.release('PRIVATE','SHARED','EXCERPTS','PUBLIC')`, `Segment`: `USER-DEFINED`}
	_              = bytes.MinRead
)

func testExcerptsUpdate(t *testing.T) {
	t.Parallel()

	if len(excerptColumns) == len(excerptPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := excerptColumnsWithDefault
	blacklist = append(blacklist, excerptColumnsWithCustom...)

	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	if err = excerpt.Update(tx); err != nil {
		t.Error(err)
	}
}

func testExcerptsSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(excerptColumns) == len(excerptPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := excerptPrimaryKeyColumns
	blacklist = append(blacklist, excerptColumnsWithCustom...)

	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(excerptColumns, excerptPrimaryKeyColumns) {
		fields = excerptColumns
	} else {
		fields = strmangle.SetComplement(
			excerptColumns,
			excerptPrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(excerpt))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := ExcerptSlice{excerpt}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}

func testExcerptsUpsert(t *testing.T) {
	t.Parallel()

	if len(excerptColumns) == len(excerptPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	excerpt := &Excerpt{}
	if err = randomize.Struct(seed, excerpt, excerptDBTypes, true, excerptColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = excerpt.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert Excerpt: %s", err)
	}

	count, err := Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	blacklist := excerptPrimaryKeyColumns

	blacklist = append(blacklist, excerptColumnsWithCustom...)

	if err = randomize.Struct(seed, excerpt, excerptDBTypes, false, blacklist...); err != nil {
		t.Errorf("Unable to randomize Excerpt struct: %s", err)
	}

	excerpt.Segment = custom_types.SegmentRandom()
	excerpt.Release = custom_types.NullReleaseRandom()

	if err = excerpt.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert Excerpt: %s", err)
	}

	count, err = Excerpts(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
