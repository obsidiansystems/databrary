// This file is generated by SQLBoiler (https://github.com/vattle/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// DO NOT EDIT

package models

import (
	"bytes"
	"reflect"
	"testing"

	"github.com/vattle/sqlboiler/boil"
	"github.com/vattle/sqlboiler/randomize"
	"github.com/vattle/sqlboiler/strmangle"
)

func testVolumeTextIdxes(t *testing.T) {
	t.Parallel()

	query := VolumeTextIdxes(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}
func testVolumeTextIdxesDelete(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = volumeTextIdx.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeTextIdxesQueryDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = VolumeTextIdxes(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeTextIdxesSliceDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := VolumeTextIdxSlice{volumeTextIdx}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}
func testVolumeTextIdxesExists(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := VolumeTextIdxExists(tx, volumeTextIdx.Volume)
	if err != nil {
		t.Errorf("Unable to check if VolumeTextIdx exists: %s", err)
	}
	if !e {
		t.Errorf("Expected VolumeTextIdxExistsG to return true, but got false.")
	}
}
func testVolumeTextIdxesFind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	volumeTextIdxFound, err := FindVolumeTextIdx(tx, volumeTextIdx.Volume)
	if err != nil {
		t.Error(err)
	}

	if volumeTextIdxFound == nil {
		t.Error("want a record, got nil")
	}
}
func testVolumeTextIdxesBind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = VolumeTextIdxes(tx).Bind(volumeTextIdx); err != nil {
		t.Error(err)
	}
}

func testVolumeTextIdxesOne(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := VolumeTextIdxes(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testVolumeTextIdxesAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdxOne := &VolumeTextIdx{}
	volumeTextIdxTwo := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdxOne, volumeTextIdxDBTypes, false, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}
	if err = randomize.Struct(seed, volumeTextIdxTwo, volumeTextIdxDBTypes, false, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdxOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = volumeTextIdxTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := VolumeTextIdxes(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testVolumeTextIdxesCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeTextIdxOne := &VolumeTextIdx{}
	volumeTextIdxTwo := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdxOne, volumeTextIdxDBTypes, false, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}
	if err = randomize.Struct(seed, volumeTextIdxTwo, volumeTextIdxDBTypes, false, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdxOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = volumeTextIdxTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}
func volumeTextIdxBeforeInsertHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func volumeTextIdxAfterInsertHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func volumeTextIdxAfterSelectHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func volumeTextIdxBeforeUpdateHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func volumeTextIdxAfterUpdateHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func volumeTextIdxBeforeDeleteHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func volumeTextIdxAfterDeleteHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func volumeTextIdxBeforeUpsertHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func volumeTextIdxAfterUpsertHook(e boil.Executor, o *VolumeTextIdx) error {
	*o = VolumeTextIdx{}
	return nil
}

func testVolumeTextIdxesHooks(t *testing.T) {
	t.Parallel()

	var err error

	empty := &VolumeTextIdx{}
	o := &VolumeTextIdx{}

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, o, volumeTextIdxDBTypes, false); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx object: %s", err)
	}

	AddVolumeTextIdxHook(boil.BeforeInsertHook, volumeTextIdxBeforeInsertHook)
	if err = o.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxBeforeInsertHooks = []VolumeTextIdxHook{}

	AddVolumeTextIdxHook(boil.AfterInsertHook, volumeTextIdxAfterInsertHook)
	if err = o.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxAfterInsertHooks = []VolumeTextIdxHook{}

	AddVolumeTextIdxHook(boil.AfterSelectHook, volumeTextIdxAfterSelectHook)
	if err = o.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxAfterSelectHooks = []VolumeTextIdxHook{}

	AddVolumeTextIdxHook(boil.BeforeUpdateHook, volumeTextIdxBeforeUpdateHook)
	if err = o.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxBeforeUpdateHooks = []VolumeTextIdxHook{}

	AddVolumeTextIdxHook(boil.AfterUpdateHook, volumeTextIdxAfterUpdateHook)
	if err = o.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxAfterUpdateHooks = []VolumeTextIdxHook{}

	AddVolumeTextIdxHook(boil.BeforeDeleteHook, volumeTextIdxBeforeDeleteHook)
	if err = o.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxBeforeDeleteHooks = []VolumeTextIdxHook{}

	AddVolumeTextIdxHook(boil.AfterDeleteHook, volumeTextIdxAfterDeleteHook)
	if err = o.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxAfterDeleteHooks = []VolumeTextIdxHook{}

	AddVolumeTextIdxHook(boil.BeforeUpsertHook, volumeTextIdxBeforeUpsertHook)
	if err = o.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxBeforeUpsertHooks = []VolumeTextIdxHook{}

	AddVolumeTextIdxHook(boil.AfterUpsertHook, volumeTextIdxAfterUpsertHook)
	if err = o.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", o)
	}
	volumeTextIdxAfterUpsertHooks = []VolumeTextIdxHook{}
}
func testVolumeTextIdxesInsert(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testVolumeTextIdxesInsertWhitelist(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx, volumeTextIdxColumns...); err != nil {
		t.Error(err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testVolumeTextIdxToOneVolumeUsingVolume(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var local VolumeTextIdx
	var foreign Volume

	seed := randomize.NewSeed()
	if err := randomize.Struct(seed, &local, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}
	if err := randomize.Struct(seed, &foreign, volumeDBTypes, true, volumeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Volume struct: %s", err)
	}

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

	slice := VolumeTextIdxSlice{&local}
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

func testVolumeTextIdxToOneSetOpVolumeUsingVolume(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a VolumeTextIdx
	var b, c Volume

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, volumeTextIdxDBTypes, false, strmangle.SetComplement(volumeTextIdxPrimaryKeyColumns, volumeTextIdxColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &b, volumeDBTypes, false, strmangle.SetComplement(volumePrimaryKeyColumns, volumeColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &c, volumeDBTypes, false, strmangle.SetComplement(volumePrimaryKeyColumns, volumeColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}

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

		if x.R.VolumeTextIdx != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Volume != x.ID {
			t.Error("foreign key was wrong value", a.Volume)
		}

		if exists, err := VolumeTextIdxExists(tx, a.Volume); err != nil {
			t.Fatal(err)
		} else if !exists {
			t.Error("want 'a' to exist")
		}

	}
}
func testVolumeTextIdxesReload(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = volumeTextIdx.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeTextIdxesReloadAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := VolumeTextIdxSlice{volumeTextIdx}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}
func testVolumeTextIdxesSelect(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := VolumeTextIdxes(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	volumeTextIdxDBTypes = map[string]string{`TS`: `tsvector`, `Volume`: `integer`}
	_                    = bytes.MinRead
)

func testVolumeTextIdxesUpdate(t *testing.T) {
	t.Parallel()

	if len(volumeTextIdxColumns) == len(volumeTextIdxPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	if err = volumeTextIdx.Update(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeTextIdxesSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(volumeTextIdxColumns) == len(volumeTextIdxPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	volumeTextIdx := &VolumeTextIdx{}
	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, volumeTextIdx, volumeTextIdxDBTypes, true, volumeTextIdxPrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(volumeTextIdxColumns, volumeTextIdxPrimaryKeyColumns) {
		fields = volumeTextIdxColumns
	} else {
		fields = strmangle.SetComplement(
			volumeTextIdxColumns,
			volumeTextIdxPrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(volumeTextIdx))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := VolumeTextIdxSlice{volumeTextIdx}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}
func testVolumeTextIdxesUpsert(t *testing.T) {
	t.Parallel()

	if len(volumeTextIdxColumns) == len(volumeTextIdxPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	// Attempt the INSERT side of an UPSERT
	volumeTextIdx := VolumeTextIdx{}
	if err = randomize.Struct(seed, &volumeTextIdx, volumeTextIdxDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeTextIdx.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert VolumeTextIdx: %s", err)
	}

	count, err := VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	if err = randomize.Struct(seed, &volumeTextIdx, volumeTextIdxDBTypes, false, volumeTextIdxPrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize VolumeTextIdx struct: %s", err)
	}

	if err = volumeTextIdx.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert VolumeTextIdx: %s", err)
	}

	count, err = VolumeTextIdxes(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
