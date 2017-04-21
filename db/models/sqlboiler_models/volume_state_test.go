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

func testVolumeStates(t *testing.T) {
	t.Parallel()

	query := VolumeStates(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}
func testVolumeStatesDelete(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = volumeState.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeStatesQueryDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = VolumeStates(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeStatesSliceDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := VolumeStateSlice{volumeState}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}
func testVolumeStatesExists(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := VolumeStateExists(tx, volumeState.Volume, volumeState.Key)
	if err != nil {
		t.Errorf("Unable to check if VolumeState exists: %s", err)
	}
	if !e {
		t.Errorf("Expected VolumeStateExistsG to return true, but got false.")
	}
}
func testVolumeStatesFind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	volumeStateFound, err := FindVolumeState(tx, volumeState.Volume, volumeState.Key)
	if err != nil {
		t.Error(err)
	}

	if volumeStateFound == nil {
		t.Error("want a record, got nil")
	}
}
func testVolumeStatesBind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = VolumeStates(tx).Bind(volumeState); err != nil {
		t.Error(err)
	}
}

func testVolumeStatesOne(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := VolumeStates(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testVolumeStatesAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeStateOne := &VolumeState{}
	volumeStateTwo := &VolumeState{}
	if err = randomize.Struct(seed, volumeStateOne, volumeStateDBTypes, false, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}
	if err = randomize.Struct(seed, volumeStateTwo, volumeStateDBTypes, false, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeStateOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = volumeStateTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := VolumeStates(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testVolumeStatesCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeStateOne := &VolumeState{}
	volumeStateTwo := &VolumeState{}
	if err = randomize.Struct(seed, volumeStateOne, volumeStateDBTypes, false, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}
	if err = randomize.Struct(seed, volumeStateTwo, volumeStateDBTypes, false, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeStateOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = volumeStateTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}
func volumeStateBeforeInsertHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func volumeStateAfterInsertHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func volumeStateAfterSelectHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func volumeStateBeforeUpdateHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func volumeStateAfterUpdateHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func volumeStateBeforeDeleteHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func volumeStateAfterDeleteHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func volumeStateBeforeUpsertHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func volumeStateAfterUpsertHook(e boil.Executor, o *VolumeState) error {
	*o = VolumeState{}
	return nil
}

func testVolumeStatesHooks(t *testing.T) {
	t.Parallel()

	var err error

	empty := &VolumeState{}
	o := &VolumeState{}

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, o, volumeStateDBTypes, false); err != nil {
		t.Errorf("Unable to randomize VolumeState object: %s", err)
	}

	AddVolumeStateHook(boil.BeforeInsertHook, volumeStateBeforeInsertHook)
	if err = o.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", o)
	}
	volumeStateBeforeInsertHooks = []VolumeStateHook{}

	AddVolumeStateHook(boil.AfterInsertHook, volumeStateAfterInsertHook)
	if err = o.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", o)
	}
	volumeStateAfterInsertHooks = []VolumeStateHook{}

	AddVolumeStateHook(boil.AfterSelectHook, volumeStateAfterSelectHook)
	if err = o.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", o)
	}
	volumeStateAfterSelectHooks = []VolumeStateHook{}

	AddVolumeStateHook(boil.BeforeUpdateHook, volumeStateBeforeUpdateHook)
	if err = o.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", o)
	}
	volumeStateBeforeUpdateHooks = []VolumeStateHook{}

	AddVolumeStateHook(boil.AfterUpdateHook, volumeStateAfterUpdateHook)
	if err = o.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", o)
	}
	volumeStateAfterUpdateHooks = []VolumeStateHook{}

	AddVolumeStateHook(boil.BeforeDeleteHook, volumeStateBeforeDeleteHook)
	if err = o.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", o)
	}
	volumeStateBeforeDeleteHooks = []VolumeStateHook{}

	AddVolumeStateHook(boil.AfterDeleteHook, volumeStateAfterDeleteHook)
	if err = o.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", o)
	}
	volumeStateAfterDeleteHooks = []VolumeStateHook{}

	AddVolumeStateHook(boil.BeforeUpsertHook, volumeStateBeforeUpsertHook)
	if err = o.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", o)
	}
	volumeStateBeforeUpsertHooks = []VolumeStateHook{}

	AddVolumeStateHook(boil.AfterUpsertHook, volumeStateAfterUpsertHook)
	if err = o.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", o)
	}
	volumeStateAfterUpsertHooks = []VolumeStateHook{}
}
func testVolumeStatesInsert(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testVolumeStatesInsertWhitelist(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx, volumeStateColumns...); err != nil {
		t.Error(err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testVolumeStateToOneVolumeUsingVolume(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var local VolumeState
	var foreign Volume

	seed := randomize.NewSeed()
	if err := randomize.Struct(seed, &local, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
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

	slice := VolumeStateSlice{&local}
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

func testVolumeStateToOneSetOpVolumeUsingVolume(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a VolumeState
	var b, c Volume

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, volumeStateDBTypes, false, strmangle.SetComplement(volumeStatePrimaryKeyColumns, volumeStateColumnsWithoutDefault)...); err != nil {
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

		if x.R.VolumeStates[0] != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Volume != x.ID {
			t.Error("foreign key was wrong value", a.Volume)
		}

		if exists, err := VolumeStateExists(tx, a.Volume, a.Key); err != nil {
			t.Fatal(err)
		} else if !exists {
			t.Error("want 'a' to exist")
		}

	}
}
func testVolumeStatesReload(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = volumeState.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeStatesReloadAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := VolumeStateSlice{volumeState}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}
func testVolumeStatesSelect(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := VolumeStates(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	volumeStateDBTypes = map[string]string{`Key`: `character varying`, `Public`: `boolean`, `Value`: `jsonb`, `Volume`: `integer`}
	_                  = bytes.MinRead
)

func testVolumeStatesUpdate(t *testing.T) {
	t.Parallel()

	if len(volumeStateColumns) == len(volumeStatePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStateColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	if err = volumeState.Update(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeStatesSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(volumeStateColumns) == len(volumeStatePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	volumeState := &VolumeState{}
	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, volumeState, volumeStateDBTypes, true, volumeStatePrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(volumeStateColumns, volumeStatePrimaryKeyColumns) {
		fields = volumeStateColumns
	} else {
		fields = strmangle.SetComplement(
			volumeStateColumns,
			volumeStatePrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(volumeState))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := VolumeStateSlice{volumeState}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}
func testVolumeStatesUpsert(t *testing.T) {
	t.Parallel()

	if len(volumeStateColumns) == len(volumeStatePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	// Attempt the INSERT side of an UPSERT
	volumeState := VolumeState{}
	if err = randomize.Struct(seed, &volumeState, volumeStateDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeState.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert VolumeState: %s", err)
	}

	count, err := VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	if err = randomize.Struct(seed, &volumeState, volumeStateDBTypes, false, volumeStatePrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize VolumeState struct: %s", err)
	}

	if err = volumeState.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert VolumeState: %s", err)
	}

	count, err = VolumeStates(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
