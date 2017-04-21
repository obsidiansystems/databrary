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

func testLoginTokens(t *testing.T) {
	t.Parallel()

	query := LoginTokens(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}
func testLoginTokensDelete(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = loginToken.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testLoginTokensQueryDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = LoginTokens(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testLoginTokensSliceDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := LoginTokenSlice{loginToken}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}
func testLoginTokensExists(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := LoginTokenExists(tx, loginToken.Token)
	if err != nil {
		t.Errorf("Unable to check if LoginToken exists: %s", err)
	}
	if !e {
		t.Errorf("Expected LoginTokenExistsG to return true, but got false.")
	}
}
func testLoginTokensFind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	loginTokenFound, err := FindLoginToken(tx, loginToken.Token)
	if err != nil {
		t.Error(err)
	}

	if loginTokenFound == nil {
		t.Error("want a record, got nil")
	}
}
func testLoginTokensBind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = LoginTokens(tx).Bind(loginToken); err != nil {
		t.Error(err)
	}
}

func testLoginTokensOne(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := LoginTokens(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testLoginTokensAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginTokenOne := &LoginToken{}
	loginTokenTwo := &LoginToken{}
	if err = randomize.Struct(seed, loginTokenOne, loginTokenDBTypes, false, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}
	if err = randomize.Struct(seed, loginTokenTwo, loginTokenDBTypes, false, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginTokenOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = loginTokenTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := LoginTokens(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testLoginTokensCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	loginTokenOne := &LoginToken{}
	loginTokenTwo := &LoginToken{}
	if err = randomize.Struct(seed, loginTokenOne, loginTokenDBTypes, false, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}
	if err = randomize.Struct(seed, loginTokenTwo, loginTokenDBTypes, false, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginTokenOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = loginTokenTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}
func loginTokenBeforeInsertHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func loginTokenAfterInsertHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func loginTokenAfterSelectHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func loginTokenBeforeUpdateHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func loginTokenAfterUpdateHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func loginTokenBeforeDeleteHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func loginTokenAfterDeleteHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func loginTokenBeforeUpsertHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func loginTokenAfterUpsertHook(e boil.Executor, o *LoginToken) error {
	*o = LoginToken{}
	return nil
}

func testLoginTokensHooks(t *testing.T) {
	t.Parallel()

	var err error

	empty := &LoginToken{}
	o := &LoginToken{}

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, o, loginTokenDBTypes, false); err != nil {
		t.Errorf("Unable to randomize LoginToken object: %s", err)
	}

	AddLoginTokenHook(boil.BeforeInsertHook, loginTokenBeforeInsertHook)
	if err = o.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", o)
	}
	loginTokenBeforeInsertHooks = []LoginTokenHook{}

	AddLoginTokenHook(boil.AfterInsertHook, loginTokenAfterInsertHook)
	if err = o.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", o)
	}
	loginTokenAfterInsertHooks = []LoginTokenHook{}

	AddLoginTokenHook(boil.AfterSelectHook, loginTokenAfterSelectHook)
	if err = o.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", o)
	}
	loginTokenAfterSelectHooks = []LoginTokenHook{}

	AddLoginTokenHook(boil.BeforeUpdateHook, loginTokenBeforeUpdateHook)
	if err = o.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", o)
	}
	loginTokenBeforeUpdateHooks = []LoginTokenHook{}

	AddLoginTokenHook(boil.AfterUpdateHook, loginTokenAfterUpdateHook)
	if err = o.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", o)
	}
	loginTokenAfterUpdateHooks = []LoginTokenHook{}

	AddLoginTokenHook(boil.BeforeDeleteHook, loginTokenBeforeDeleteHook)
	if err = o.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", o)
	}
	loginTokenBeforeDeleteHooks = []LoginTokenHook{}

	AddLoginTokenHook(boil.AfterDeleteHook, loginTokenAfterDeleteHook)
	if err = o.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", o)
	}
	loginTokenAfterDeleteHooks = []LoginTokenHook{}

	AddLoginTokenHook(boil.BeforeUpsertHook, loginTokenBeforeUpsertHook)
	if err = o.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", o)
	}
	loginTokenBeforeUpsertHooks = []LoginTokenHook{}

	AddLoginTokenHook(boil.AfterUpsertHook, loginTokenAfterUpsertHook)
	if err = o.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", o)
	}
	loginTokenAfterUpsertHooks = []LoginTokenHook{}
}
func testLoginTokensInsert(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testLoginTokensInsertWhitelist(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx, loginTokenColumns...); err != nil {
		t.Error(err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testLoginTokenToOneAccountUsingAccount(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var local LoginToken
	var foreign Account

	seed := randomize.NewSeed()
	if err := randomize.Struct(seed, &local, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}
	if err := randomize.Struct(seed, &foreign, accountDBTypes, true, accountColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Account struct: %s", err)
	}

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Account = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.AccountByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := LoginTokenSlice{&local}
	if err = local.L.LoadAccount(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Account == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Account = nil
	if err = local.L.LoadAccount(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Account == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testLoginTokenToOneSetOpAccountUsingAccount(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a LoginToken
	var b, c Account

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, loginTokenDBTypes, false, strmangle.SetComplement(loginTokenPrimaryKeyColumns, loginTokenColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &b, accountDBTypes, false, strmangle.SetComplement(accountPrimaryKeyColumns, accountColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &c, accountDBTypes, false, strmangle.SetComplement(accountPrimaryKeyColumns, accountColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Account{&b, &c} {
		err = a.SetAccount(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Account != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.LoginToken != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Account != x.ID {
			t.Error("foreign key was wrong value", a.Account)
		}

		zero := reflect.Zero(reflect.TypeOf(a.Account))
		reflect.Indirect(reflect.ValueOf(&a.Account)).Set(zero)

		if err = a.Reload(tx); err != nil {
			t.Fatal("failed to reload", err)
		}

		if a.Account != x.ID {
			t.Error("foreign key was wrong value", a.Account, x.ID)
		}
	}
}
func testLoginTokensReload(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = loginToken.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testLoginTokensReloadAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := LoginTokenSlice{loginToken}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}
func testLoginTokensSelect(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := LoginTokens(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	loginTokenDBTypes = map[string]string{`Account`: `integer`, `Expires`: `timestamp with time zone`, `Password`: `boolean`, `Token`: `character varying`}
	_                 = bytes.MinRead
)

func testLoginTokensUpdate(t *testing.T) {
	t.Parallel()

	if len(loginTokenColumns) == len(loginTokenPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	if err = loginToken.Update(tx); err != nil {
		t.Error(err)
	}
}

func testLoginTokensSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(loginTokenColumns) == len(loginTokenPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	loginToken := &LoginToken{}
	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, loginToken, loginTokenDBTypes, true, loginTokenPrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(loginTokenColumns, loginTokenPrimaryKeyColumns) {
		fields = loginTokenColumns
	} else {
		fields = strmangle.SetComplement(
			loginTokenColumns,
			loginTokenPrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(loginToken))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := LoginTokenSlice{loginToken}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}
func testLoginTokensUpsert(t *testing.T) {
	t.Parallel()

	if len(loginTokenColumns) == len(loginTokenPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	// Attempt the INSERT side of an UPSERT
	loginToken := LoginToken{}
	if err = randomize.Struct(seed, &loginToken, loginTokenDBTypes, true); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = loginToken.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert LoginToken: %s", err)
	}

	count, err := LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	if err = randomize.Struct(seed, &loginToken, loginTokenDBTypes, false, loginTokenPrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize LoginToken struct: %s", err)
	}

	if err = loginToken.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert LoginToken: %s", err)
	}

	count, err = LoginTokens(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
