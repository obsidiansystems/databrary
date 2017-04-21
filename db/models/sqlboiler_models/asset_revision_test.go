// This file is generated by SQLBoiler (https://github.com/vattle/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// DO NOT EDIT

package models

import (
	"bytes"
	"fmt"
	"reflect"
	"testing"

	"github.com/vattle/sqlboiler/boil"
	"github.com/vattle/sqlboiler/randomize"
	"github.com/vattle/sqlboiler/strmangle"
)

func testAssetRevisions(t *testing.T) {
	t.Parallel()

	query := AssetRevisions(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}
func testAssetRevisionsDelete(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}
	fmt.Println(assetRevision)
	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = assetRevision.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testAssetRevisionsQueryDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = AssetRevisions(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testAssetRevisionsSliceDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := AssetRevisionSlice{assetRevision}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}
func testAssetRevisionsExists(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := AssetRevisionExists(tx, assetRevision.Asset)
	if err != nil {
		t.Errorf("Unable to check if AssetRevision exists: %s", err)
	}
	if !e {
		t.Errorf("Expected AssetRevisionExistsG to return true, but got false.")
	}
}
func testAssetRevisionsFind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	assetRevisionFound, err := FindAssetRevision(tx, assetRevision.Asset)
	if err != nil {
		t.Error(err)
	}

	if assetRevisionFound == nil {
		t.Error("want a record, got nil")
	}
}
func testAssetRevisionsBind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = AssetRevisions(tx).Bind(assetRevision); err != nil {
		t.Error(err)
	}
}

func testAssetRevisionsOne(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := AssetRevisions(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testAssetRevisionsAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevisionOne := &AssetRevision{}
	assetRevisionTwo := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevisionOne, assetRevisionDBTypes, false, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}
	if err = randomize.Struct(seed, assetRevisionTwo, assetRevisionDBTypes, false, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevisionOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = assetRevisionTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := AssetRevisions(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testAssetRevisionsCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	assetRevisionOne := &AssetRevision{}
	assetRevisionTwo := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevisionOne, assetRevisionDBTypes, false, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}
	if err = randomize.Struct(seed, assetRevisionTwo, assetRevisionDBTypes, false, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevisionOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = assetRevisionTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}
func assetRevisionBeforeInsertHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func assetRevisionAfterInsertHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func assetRevisionAfterSelectHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func assetRevisionBeforeUpdateHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func assetRevisionAfterUpdateHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func assetRevisionBeforeDeleteHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func assetRevisionAfterDeleteHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func assetRevisionBeforeUpsertHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func assetRevisionAfterUpsertHook(e boil.Executor, o *AssetRevision) error {
	*o = AssetRevision{}
	return nil
}

func testAssetRevisionsHooks(t *testing.T) {
	t.Parallel()

	var err error

	empty := &AssetRevision{}
	o := &AssetRevision{}

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, o, assetRevisionDBTypes, false); err != nil {
		t.Errorf("Unable to randomize AssetRevision object: %s", err)
	}

	AddAssetRevisionHook(boil.BeforeInsertHook, assetRevisionBeforeInsertHook)
	if err = o.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", o)
	}
	assetRevisionBeforeInsertHooks = []AssetRevisionHook{}

	AddAssetRevisionHook(boil.AfterInsertHook, assetRevisionAfterInsertHook)
	if err = o.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", o)
	}
	assetRevisionAfterInsertHooks = []AssetRevisionHook{}

	AddAssetRevisionHook(boil.AfterSelectHook, assetRevisionAfterSelectHook)
	if err = o.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", o)
	}
	assetRevisionAfterSelectHooks = []AssetRevisionHook{}

	AddAssetRevisionHook(boil.BeforeUpdateHook, assetRevisionBeforeUpdateHook)
	if err = o.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", o)
	}
	assetRevisionBeforeUpdateHooks = []AssetRevisionHook{}

	AddAssetRevisionHook(boil.AfterUpdateHook, assetRevisionAfterUpdateHook)
	if err = o.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", o)
	}
	assetRevisionAfterUpdateHooks = []AssetRevisionHook{}

	AddAssetRevisionHook(boil.BeforeDeleteHook, assetRevisionBeforeDeleteHook)
	if err = o.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", o)
	}
	assetRevisionBeforeDeleteHooks = []AssetRevisionHook{}

	AddAssetRevisionHook(boil.AfterDeleteHook, assetRevisionAfterDeleteHook)
	if err = o.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", o)
	}
	assetRevisionAfterDeleteHooks = []AssetRevisionHook{}

	AddAssetRevisionHook(boil.BeforeUpsertHook, assetRevisionBeforeUpsertHook)
	if err = o.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", o)
	}
	assetRevisionBeforeUpsertHooks = []AssetRevisionHook{}

	AddAssetRevisionHook(boil.AfterUpsertHook, assetRevisionAfterUpsertHook)
	if err = o.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", o)
	}
	assetRevisionAfterUpsertHooks = []AssetRevisionHook{}
}
func testAssetRevisionsInsert(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testAssetRevisionsInsertWhitelist(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx, assetRevisionColumns...); err != nil {
		t.Error(err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testAssetRevisionToOneAssetUsingAsset(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var local AssetRevision
	var foreign Asset

	seed := randomize.NewSeed()
	if err := randomize.Struct(seed, &local, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}
	if err := randomize.Struct(seed, &foreign, assetDBTypes, true, assetColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Asset struct: %s", err)
	}

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Asset = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.AssetByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := AssetRevisionSlice{&local}
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

func testAssetRevisionToOneAssetUsingOrig(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var local AssetRevision
	var foreign Asset

	seed := randomize.NewSeed()
	if err := randomize.Struct(seed, &local, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}
	if err := randomize.Struct(seed, &foreign, assetDBTypes, true, assetColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Asset struct: %s", err)
	}

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Orig = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.OrigByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := AssetRevisionSlice{&local}
	if err = local.L.LoadOrig(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Orig == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Orig = nil
	if err = local.L.LoadOrig(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Orig == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testAssetRevisionToOneSetOpAssetUsingAsset(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a AssetRevision
	var b, c Asset

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, assetRevisionDBTypes, false, strmangle.SetComplement(assetRevisionPrimaryKeyColumns, assetRevisionColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &b, assetDBTypes, false, strmangle.SetComplement(assetPrimaryKeyColumns, assetColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &c, assetDBTypes, false, strmangle.SetComplement(assetPrimaryKeyColumns, assetColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Asset{&b, &c} {
		err = a.SetAsset(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Asset != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.AssetRevision != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Asset != x.ID {
			t.Error("foreign key was wrong value", a.Asset)
		}

		if exists, err := AssetRevisionExists(tx, a.Asset); err != nil {
			t.Fatal(err)
		} else if !exists {
			t.Error("want 'a' to exist")
		}

	}
}
func testAssetRevisionToOneSetOpAssetUsingOrig(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a AssetRevision
	var b, c Asset

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, assetRevisionDBTypes, false, strmangle.SetComplement(assetRevisionPrimaryKeyColumns, assetRevisionColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &b, assetDBTypes, false, strmangle.SetComplement(assetPrimaryKeyColumns, assetColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &c, assetDBTypes, false, strmangle.SetComplement(assetPrimaryKeyColumns, assetColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Asset{&b, &c} {
		err = a.SetOrig(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Orig != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.OrigAssetRevisions[0] != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Orig != x.ID {
			t.Error("foreign key was wrong value", a.Orig)
		}

		zero := reflect.Zero(reflect.TypeOf(a.Orig))
		reflect.Indirect(reflect.ValueOf(&a.Orig)).Set(zero)

		if err = a.Reload(tx); err != nil {
			t.Fatal("failed to reload", err)
		}

		if a.Orig != x.ID {
			t.Error("foreign key was wrong value", a.Orig, x.ID)
		}
	}
}
func testAssetRevisionsReload(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = assetRevision.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testAssetRevisionsReloadAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := AssetRevisionSlice{assetRevision}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}
func testAssetRevisionsSelect(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := AssetRevisions(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	assetRevisionDBTypes = map[string]string{`Asset`: `integer`, `Orig`: `integer`}
	_                    = bytes.MinRead
)

func testAssetRevisionsUpdate(t *testing.T) {
	t.Parallel()

	if len(assetRevisionColumns) == len(assetRevisionPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	if err = assetRevision.Update(tx); err != nil {
		t.Error(err)
	}
}

func testAssetRevisionsSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(assetRevisionColumns) == len(assetRevisionPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	assetRevision := &AssetRevision{}
	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, assetRevision, assetRevisionDBTypes, true, assetRevisionPrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(assetRevisionColumns, assetRevisionPrimaryKeyColumns) {
		fields = assetRevisionColumns
	} else {
		fields = strmangle.SetComplement(
			assetRevisionColumns,
			assetRevisionPrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(assetRevision))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := AssetRevisionSlice{assetRevision}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}
func testAssetRevisionsUpsert(t *testing.T) {
	t.Parallel()

	if len(assetRevisionColumns) == len(assetRevisionPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	// Attempt the INSERT side of an UPSERT
	assetRevision := AssetRevision{}
	if err = randomize.Struct(seed, &assetRevision, assetRevisionDBTypes, true); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = assetRevision.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert AssetRevision: %s", err)
	}

	count, err := AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	if err = randomize.Struct(seed, &assetRevision, assetRevisionDBTypes, false, assetRevisionPrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize AssetRevision struct: %s", err)
	}

	if err = assetRevision.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert AssetRevision: %s", err)
	}

	count, err = AssetRevisions(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
