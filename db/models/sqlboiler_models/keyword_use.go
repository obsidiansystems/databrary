// This file is generated by SQLBoiler (https://github.com/vattle/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// DO NOT EDIT

package models

import (
	"bytes"
	"database/sql"
	"fmt"
	"reflect"
	"strings"
	"sync"
	"time"

	"github.com/pkg/errors"
	"github.com/vattle/sqlboiler/boil"
	"github.com/vattle/sqlboiler/queries"
	"github.com/vattle/sqlboiler/queries/qm"
	"github.com/vattle/sqlboiler/strmangle"
)

// KeywordUse is an object representing the database table.
type KeywordUse struct {
	Container int    `boil:"container" json:"container" toml:"container" yaml:"container"`
	Segment   string `boil:"segment" json:"segment" toml:"segment" yaml:"segment"`
	Tag       int    `boil:"tag" json:"tag" toml:"tag" yaml:"tag"`
	Who       int    `boil:"who" json:"who" toml:"who" yaml:"who"`

	R *keywordUseR `boil:"-" json:"-" toml:"-" yaml:"-"`
	L keywordUseL  `boil:"-" json:"-" toml:"-" yaml:"-"`
}

// keywordUseR is where relationships are stored.
type keywordUseR struct {
}

// keywordUseL is where Load methods for each relationship are stored.
type keywordUseL struct{}

var (
	keywordUseColumns               = []string{"container", "segment", "tag", "who"}
	keywordUseColumnsWithoutDefault = []string{"container", "segment", "tag", "who"}
	keywordUseColumnsWithDefault    = []string{}
	keywordUsePrimaryKeyColumns     = []string{"container", "segment", "tag"}
)

type (
	// KeywordUseSlice is an alias for a slice of pointers to KeywordUse.
	// This should generally be used opposed to []KeywordUse.
	KeywordUseSlice []*KeywordUse
	// KeywordUseHook is the signature for custom KeywordUse hook methods
	KeywordUseHook func(boil.Executor, *KeywordUse) error

	keywordUseQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	keywordUseType                 = reflect.TypeOf(&KeywordUse{})
	keywordUseMapping              = queries.MakeStructMapping(keywordUseType)
	keywordUsePrimaryKeyMapping, _ = queries.BindMapping(keywordUseType, keywordUseMapping, keywordUsePrimaryKeyColumns)
	keywordUseInsertCacheMut       sync.RWMutex
	keywordUseInsertCache          = make(map[string]insertCache)
	keywordUseUpdateCacheMut       sync.RWMutex
	keywordUseUpdateCache          = make(map[string]updateCache)
	keywordUseUpsertCacheMut       sync.RWMutex
	keywordUseUpsertCache          = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var keywordUseBeforeInsertHooks []KeywordUseHook
var keywordUseBeforeUpdateHooks []KeywordUseHook
var keywordUseBeforeDeleteHooks []KeywordUseHook
var keywordUseBeforeUpsertHooks []KeywordUseHook

var keywordUseAfterInsertHooks []KeywordUseHook
var keywordUseAfterSelectHooks []KeywordUseHook
var keywordUseAfterUpdateHooks []KeywordUseHook
var keywordUseAfterDeleteHooks []KeywordUseHook
var keywordUseAfterUpsertHooks []KeywordUseHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *KeywordUse) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *KeywordUse) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *KeywordUse) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *KeywordUse) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *KeywordUse) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *KeywordUse) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *KeywordUse) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *KeywordUse) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *KeywordUse) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range keywordUseAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddKeywordUseHook registers your hook function for all future operations.
func AddKeywordUseHook(hookPoint boil.HookPoint, keywordUseHook KeywordUseHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		keywordUseBeforeInsertHooks = append(keywordUseBeforeInsertHooks, keywordUseHook)
	case boil.BeforeUpdateHook:
		keywordUseBeforeUpdateHooks = append(keywordUseBeforeUpdateHooks, keywordUseHook)
	case boil.BeforeDeleteHook:
		keywordUseBeforeDeleteHooks = append(keywordUseBeforeDeleteHooks, keywordUseHook)
	case boil.BeforeUpsertHook:
		keywordUseBeforeUpsertHooks = append(keywordUseBeforeUpsertHooks, keywordUseHook)
	case boil.AfterInsertHook:
		keywordUseAfterInsertHooks = append(keywordUseAfterInsertHooks, keywordUseHook)
	case boil.AfterSelectHook:
		keywordUseAfterSelectHooks = append(keywordUseAfterSelectHooks, keywordUseHook)
	case boil.AfterUpdateHook:
		keywordUseAfterUpdateHooks = append(keywordUseAfterUpdateHooks, keywordUseHook)
	case boil.AfterDeleteHook:
		keywordUseAfterDeleteHooks = append(keywordUseAfterDeleteHooks, keywordUseHook)
	case boil.AfterUpsertHook:
		keywordUseAfterUpsertHooks = append(keywordUseAfterUpsertHooks, keywordUseHook)
	}
}

// OneP returns a single keywordUse record from the query, and panics on error.
func (q keywordUseQuery) OneP() *KeywordUse {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single keywordUse record from the query.
func (q keywordUseQuery) One() (*KeywordUse, error) {
	o := &KeywordUse{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for keyword_use")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all KeywordUse records from the query, and panics on error.
func (q keywordUseQuery) AllP() KeywordUseSlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all KeywordUse records from the query.
func (q keywordUseQuery) All() (KeywordUseSlice, error) {
	var o KeywordUseSlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to KeywordUse slice")
	}

	if len(keywordUseAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all KeywordUse records in the query, and panics on error.
func (q keywordUseQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all KeywordUse records in the query.
func (q keywordUseQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count keyword_use rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q keywordUseQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q keywordUseQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if keyword_use exists")
	}

	return count > 0, nil
}

// KeywordUsesG retrieves all records.
func KeywordUsesG(mods ...qm.QueryMod) keywordUseQuery {
	return KeywordUses(boil.GetDB(), mods...)
}

// KeywordUses retrieves all the records using an executor.
func KeywordUses(exec boil.Executor, mods ...qm.QueryMod) keywordUseQuery {
	mods = append(mods, qm.From("\"keyword_use\""))
	return keywordUseQuery{NewQuery(exec, mods...)}
}

// FindKeywordUseG retrieves a single record by ID.
func FindKeywordUseG(container int, segment string, tag int, selectCols ...string) (*KeywordUse, error) {
	return FindKeywordUse(boil.GetDB(), container, segment, tag, selectCols...)
}

// FindKeywordUseGP retrieves a single record by ID, and panics on error.
func FindKeywordUseGP(container int, segment string, tag int, selectCols ...string) *KeywordUse {
	retobj, err := FindKeywordUse(boil.GetDB(), container, segment, tag, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// FindKeywordUse retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindKeywordUse(exec boil.Executor, container int, segment string, tag int, selectCols ...string) (*KeywordUse, error) {
	keywordUseObj := &KeywordUse{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"keyword_use\" where \"container\"=$1 AND \"segment\"=$2 AND \"tag\"=$3", sel,
	)

	q := queries.Raw(exec, query, container, segment, tag)

	err := q.Bind(keywordUseObj)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from keyword_use")
	}

	return keywordUseObj, nil
}

// FindKeywordUseP retrieves a single record by ID with an executor, and panics on error.
func FindKeywordUseP(exec boil.Executor, container int, segment string, tag int, selectCols ...string) *KeywordUse {
	retobj, err := FindKeywordUse(exec, container, segment, tag, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *KeywordUse) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *KeywordUse) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *KeywordUse) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *KeywordUse) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no keyword_use provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(keywordUseColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	keywordUseInsertCacheMut.RLock()
	cache, cached := keywordUseInsertCache[key]
	keywordUseInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			keywordUseColumns,
			keywordUseColumnsWithDefault,
			keywordUseColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(keywordUseType, keywordUseMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(keywordUseType, keywordUseMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"keyword_use\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"keyword_use\" DEFAULT VALUES"
		}

		if len(cache.retMapping) != 0 {
			cache.query += fmt.Sprintf(" RETURNING \"%s\"", strings.Join(returnColumns, "\",\""))
		}
	}

	value := reflect.Indirect(reflect.ValueOf(o))
	vals := queries.ValuesFromMapping(value, cache.valueMapping)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, cache.query)
		fmt.Fprintln(boil.DebugWriter, vals)
	}

	if len(cache.retMapping) != 0 {
		err = exec.QueryRow(cache.query, vals...).Scan(queries.PtrsFromMapping(value, cache.retMapping)...)
	} else {
		_, err = exec.Exec(cache.query, vals...)
	}

	if err != nil {
		return errors.Wrap(err, "models: unable to insert into keyword_use")
	}

	if !cached {
		keywordUseInsertCacheMut.Lock()
		keywordUseInsertCache[key] = cache
		keywordUseInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}

// UpdateG a single KeywordUse record. See Update for
// whitelist behavior description.
func (o *KeywordUse) UpdateG(whitelist ...string) error {
	return o.Update(boil.GetDB(), whitelist...)
}

// UpdateGP a single KeywordUse record.
// UpdateGP takes a whitelist of column names that should be updated.
// Panics on error. See Update for whitelist behavior description.
func (o *KeywordUse) UpdateGP(whitelist ...string) {
	if err := o.Update(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateP uses an executor to update the KeywordUse, and panics on error.
// See Update for whitelist behavior description.
func (o *KeywordUse) UpdateP(exec boil.Executor, whitelist ...string) {
	err := o.Update(exec, whitelist...)
	if err != nil {
		panic(boil.WrapErr(err))
	}
}

// Update uses an executor to update the KeywordUse.
// Whitelist behavior: If a whitelist is provided, only the columns given are updated.
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns are inferred to start with
// - All primary keys are subtracted from this set
// Update does not automatically update the record in case of default values. Use .Reload()
// to refresh the records.
func (o *KeywordUse) Update(exec boil.Executor, whitelist ...string) error {
	var err error
	if err = o.doBeforeUpdateHooks(exec); err != nil {
		return err
	}
	key := makeCacheKey(whitelist, nil)
	keywordUseUpdateCacheMut.RLock()
	cache, cached := keywordUseUpdateCache[key]
	keywordUseUpdateCacheMut.RUnlock()

	if !cached {
		wl := strmangle.UpdateColumnSet(keywordUseColumns, keywordUsePrimaryKeyColumns, whitelist)
		if len(whitelist) == 0 {
			wl = strmangle.SetComplement(wl, []string{"created_at"})
		}
		if len(wl) == 0 {
			return errors.New("models: unable to update keyword_use, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"keyword_use\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, keywordUsePrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(keywordUseType, keywordUseMapping, append(wl, keywordUsePrimaryKeyColumns...))
		if err != nil {
			return err
		}
	}

	values := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), cache.valueMapping)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, cache.query)
		fmt.Fprintln(boil.DebugWriter, values)
	}

	_, err = exec.Exec(cache.query, values...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update keyword_use row")
	}

	if !cached {
		keywordUseUpdateCacheMut.Lock()
		keywordUseUpdateCache[key] = cache
		keywordUseUpdateCacheMut.Unlock()
	}

	return o.doAfterUpdateHooks(exec)
}

// UpdateAllP updates all rows with matching column names, and panics on error.
func (q keywordUseQuery) UpdateAllP(cols M) {
	if err := q.UpdateAll(cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values.
func (q keywordUseQuery) UpdateAll(cols M) error {
	queries.SetUpdate(q.Query, cols)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to update all for keyword_use")
	}

	return nil
}

// UpdateAllG updates all rows with the specified column values.
func (o KeywordUseSlice) UpdateAllG(cols M) error {
	return o.UpdateAll(boil.GetDB(), cols)
}

// UpdateAllGP updates all rows with the specified column values, and panics on error.
func (o KeywordUseSlice) UpdateAllGP(cols M) {
	if err := o.UpdateAll(boil.GetDB(), cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAllP updates all rows with the specified column values, and panics on error.
func (o KeywordUseSlice) UpdateAllP(exec boil.Executor, cols M) {
	if err := o.UpdateAll(exec, cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o KeywordUseSlice) UpdateAll(exec boil.Executor, cols M) error {
	ln := int64(len(o))
	if ln == 0 {
		return nil
	}

	if len(cols) == 0 {
		return errors.New("models: update all requires at least one column argument")
	}

	colNames := make([]string, len(cols))
	args := make([]interface{}, len(cols))

	i := 0
	for name, value := range cols {
		colNames[i] = name
		args[i] = value
		i++
	}

	// Append all of the primary key values for each column
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), keywordUsePrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf(
		"UPDATE \"keyword_use\" SET %s WHERE (\"container\",\"segment\",\"tag\") IN (%s)",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(keywordUsePrimaryKeyColumns), len(colNames)+1, len(keywordUsePrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, sql)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(sql, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update all in keywordUse slice")
	}

	return nil
}

// UpsertG attempts an insert, and does an update or ignore on conflict.
func (o *KeywordUse) UpsertG(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	return o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...)
}

// UpsertGP attempts an insert, and does an update or ignore on conflict. Panics on error.
func (o *KeywordUse) UpsertGP(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpsertP attempts an insert using an executor, and does an update or ignore on conflict.
// UpsertP panics on error.
func (o *KeywordUse) UpsertP(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(exec, updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
func (o *KeywordUse) Upsert(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no keyword_use provided for upsert")
	}

	if err := o.doBeforeUpsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(keywordUseColumnsWithDefault, o)

	// Build cache key in-line uglily - mysql vs postgres problems
	buf := strmangle.GetBuffer()
	if updateOnConflict {
		buf.WriteByte('t')
	} else {
		buf.WriteByte('f')
	}
	buf.WriteByte('.')
	for _, c := range conflictColumns {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	for _, c := range updateColumns {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	for _, c := range whitelist {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	for _, c := range nzDefaults {
		buf.WriteString(c)
	}
	key := buf.String()
	strmangle.PutBuffer(buf)

	keywordUseUpsertCacheMut.RLock()
	cache, cached := keywordUseUpsertCache[key]
	keywordUseUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		var ret []string
		whitelist, ret = strmangle.InsertColumnSet(
			keywordUseColumns,
			keywordUseColumnsWithDefault,
			keywordUseColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)
		update := strmangle.UpdateColumnSet(
			keywordUseColumns,
			keywordUsePrimaryKeyColumns,
			updateColumns,
		)
		if len(update) == 0 {
			return errors.New("models: unable to upsert keyword_use, could not build update column list")
		}

		conflict := conflictColumns
		if len(conflict) == 0 {
			conflict = make([]string, len(keywordUsePrimaryKeyColumns))
			copy(conflict, keywordUsePrimaryKeyColumns)
		}
		cache.query = queries.BuildUpsertQueryPostgres(dialect, "\"keyword_use\"", updateOnConflict, ret, update, conflict, whitelist)

		cache.valueMapping, err = queries.BindMapping(keywordUseType, keywordUseMapping, whitelist)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(keywordUseType, keywordUseMapping, ret)
			if err != nil {
				return err
			}
		}
	}

	value := reflect.Indirect(reflect.ValueOf(o))
	vals := queries.ValuesFromMapping(value, cache.valueMapping)
	var returns []interface{}
	if len(cache.retMapping) != 0 {
		returns = queries.PtrsFromMapping(value, cache.retMapping)
	}

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, cache.query)
		fmt.Fprintln(boil.DebugWriter, vals)
	}

	if len(cache.retMapping) != 0 {
		err = exec.QueryRow(cache.query, vals...).Scan(returns...)
		if err == sql.ErrNoRows {
			err = nil // Postgres doesn't return anything when there's no update
		}
	} else {
		_, err = exec.Exec(cache.query, vals...)
	}
	if err != nil {
		return errors.Wrap(err, "models: unable to upsert keyword_use")
	}

	if !cached {
		keywordUseUpsertCacheMut.Lock()
		keywordUseUpsertCache[key] = cache
		keywordUseUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(exec)
}

// DeleteP deletes a single KeywordUse record with an executor.
// DeleteP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *KeywordUse) DeleteP(exec boil.Executor) {
	if err := o.Delete(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteG deletes a single KeywordUse record.
// DeleteG will match against the primary key column to find the record to delete.
func (o *KeywordUse) DeleteG() error {
	if o == nil {
		return errors.New("models: no KeywordUse provided for deletion")
	}

	return o.Delete(boil.GetDB())
}

// DeleteGP deletes a single KeywordUse record.
// DeleteGP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *KeywordUse) DeleteGP() {
	if err := o.DeleteG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Delete deletes a single KeywordUse record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *KeywordUse) Delete(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no KeywordUse provided for delete")
	}

	if err := o.doBeforeDeleteHooks(exec); err != nil {
		return err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), keywordUsePrimaryKeyMapping)
	sql := "DELETE FROM \"keyword_use\" WHERE \"container\"=$1 AND \"segment\"=$2 AND \"tag\"=$3"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, sql)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(sql, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete from keyword_use")
	}

	if err := o.doAfterDeleteHooks(exec); err != nil {
		return err
	}

	return nil
}

// DeleteAllP deletes all rows, and panics on error.
func (q keywordUseQuery) DeleteAllP() {
	if err := q.DeleteAll(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all matching rows.
func (q keywordUseQuery) DeleteAll() error {
	if q.Query == nil {
		return errors.New("models: no keywordUseQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from keyword_use")
	}

	return nil
}

// DeleteAllGP deletes all rows in the slice, and panics on error.
func (o KeywordUseSlice) DeleteAllGP() {
	if err := o.DeleteAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAllG deletes all rows in the slice.
func (o KeywordUseSlice) DeleteAllG() error {
	if o == nil {
		return errors.New("models: no KeywordUse slice provided for delete all")
	}
	return o.DeleteAll(boil.GetDB())
}

// DeleteAllP deletes all rows in the slice, using an executor, and panics on error.
func (o KeywordUseSlice) DeleteAllP(exec boil.Executor) {
	if err := o.DeleteAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o KeywordUseSlice) DeleteAll(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no KeywordUse slice provided for delete all")
	}

	if len(o) == 0 {
		return nil
	}

	if len(keywordUseBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), keywordUsePrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf(
		"DELETE FROM \"keyword_use\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, keywordUsePrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(keywordUsePrimaryKeyColumns), 1, len(keywordUsePrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, sql)
		fmt.Fprintln(boil.DebugWriter, args)
	}

	_, err := exec.Exec(sql, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from keywordUse slice")
	}

	if len(keywordUseAfterDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	return nil
}

// ReloadGP refetches the object from the database and panics on error.
func (o *KeywordUse) ReloadGP() {
	if err := o.ReloadG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadP refetches the object from the database with an executor. Panics on error.
func (o *KeywordUse) ReloadP(exec boil.Executor) {
	if err := o.Reload(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadG refetches the object from the database using the primary keys.
func (o *KeywordUse) ReloadG() error {
	if o == nil {
		return errors.New("models: no KeywordUse provided for reload")
	}

	return o.Reload(boil.GetDB())
}

// Reload refetches the object from the database
// using the primary keys with an executor.
func (o *KeywordUse) Reload(exec boil.Executor) error {
	ret, err := FindKeywordUse(exec, o.Container, o.Segment, o.Tag)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAllGP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *KeywordUseSlice) ReloadAllGP() {
	if err := o.ReloadAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *KeywordUseSlice) ReloadAllP(exec boil.Executor) {
	if err := o.ReloadAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllG refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *KeywordUseSlice) ReloadAllG() error {
	if o == nil {
		return errors.New("models: empty KeywordUseSlice provided for reload all")
	}

	return o.ReloadAll(boil.GetDB())
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *KeywordUseSlice) ReloadAll(exec boil.Executor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	keywordUses := KeywordUseSlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), keywordUsePrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf(
		"SELECT \"keyword_use\".* FROM \"keyword_use\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, keywordUsePrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(*o)*len(keywordUsePrimaryKeyColumns), 1, len(keywordUsePrimaryKeyColumns)),
	)

	q := queries.Raw(exec, sql, args...)

	err := q.Bind(&keywordUses)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in KeywordUseSlice")
	}

	*o = keywordUses

	return nil
}

// KeywordUseExists checks if the KeywordUse row exists.
func KeywordUseExists(exec boil.Executor, container int, segment string, tag int) (bool, error) {
	var exists bool

	sql := "select exists(select 1 from \"keyword_use\" where \"container\"=$1 AND \"segment\"=$2 AND \"tag\"=$3 limit 1)"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, sql)
		fmt.Fprintln(boil.DebugWriter, container, segment, tag)
	}

	row := exec.QueryRow(sql, container, segment, tag)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if keyword_use exists")
	}

	return exists, nil
}

// KeywordUseExistsG checks if the KeywordUse row exists.
func KeywordUseExistsG(container int, segment string, tag int) (bool, error) {
	return KeywordUseExists(boil.GetDB(), container, segment, tag)
}

// KeywordUseExistsGP checks if the KeywordUse row exists. Panics on error.
func KeywordUseExistsGP(container int, segment string, tag int) bool {
	e, err := KeywordUseExists(boil.GetDB(), container, segment, tag)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// KeywordUseExistsP checks if the KeywordUse row exists. Panics on error.
func KeywordUseExistsP(exec boil.Executor, container int, segment string, tag int) bool {
	e, err := KeywordUseExists(exec, container, segment, tag)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}
