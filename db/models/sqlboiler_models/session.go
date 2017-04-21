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

// Session is an object representing the database table.
type Session struct {
	Token     string    `boil:"token" json:"token" toml:"token" yaml:"token"`
	Expires   time.Time `boil:"expires" json:"expires" toml:"expires" yaml:"expires"`
	Account   int       `boil:"account" json:"account" toml:"account" yaml:"account"`
	Verf      string    `boil:"verf" json:"verf" toml:"verf" yaml:"verf"`
	Superuser bool      `boil:"superuser" json:"superuser" toml:"superuser" yaml:"superuser"`

	R *sessionR `boil:"-" json:"-" toml:"-" yaml:"-"`
	L sessionL  `boil:"-" json:"-" toml:"-" yaml:"-"`
}

// sessionR is where relationships are stored.
type sessionR struct {
	Account *Account
}

// sessionL is where Load methods for each relationship are stored.
type sessionL struct{}

var (
	sessionColumns               = []string{"token", "expires", "account", "verf", "superuser"}
	sessionColumnsWithoutDefault = []string{"token", "account", "verf"}
	sessionColumnsWithDefault    = []string{"expires", "superuser"}
	sessionPrimaryKeyColumns     = []string{"token"}
)

type (
	// SessionSlice is an alias for a slice of pointers to Session.
	// This should generally be used opposed to []Session.
	SessionSlice []*Session
	// SessionHook is the signature for custom Session hook methods
	SessionHook func(boil.Executor, *Session) error

	sessionQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	sessionType                 = reflect.TypeOf(&Session{})
	sessionMapping              = queries.MakeStructMapping(sessionType)
	sessionPrimaryKeyMapping, _ = queries.BindMapping(sessionType, sessionMapping, sessionPrimaryKeyColumns)
	sessionInsertCacheMut       sync.RWMutex
	sessionInsertCache          = make(map[string]insertCache)
	sessionUpdateCacheMut       sync.RWMutex
	sessionUpdateCache          = make(map[string]updateCache)
	sessionUpsertCacheMut       sync.RWMutex
	sessionUpsertCache          = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var sessionBeforeInsertHooks []SessionHook
var sessionBeforeUpdateHooks []SessionHook
var sessionBeforeDeleteHooks []SessionHook
var sessionBeforeUpsertHooks []SessionHook

var sessionAfterInsertHooks []SessionHook
var sessionAfterSelectHooks []SessionHook
var sessionAfterUpdateHooks []SessionHook
var sessionAfterDeleteHooks []SessionHook
var sessionAfterUpsertHooks []SessionHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *Session) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *Session) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *Session) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *Session) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *Session) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *Session) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *Session) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *Session) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *Session) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range sessionAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddSessionHook registers your hook function for all future operations.
func AddSessionHook(hookPoint boil.HookPoint, sessionHook SessionHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		sessionBeforeInsertHooks = append(sessionBeforeInsertHooks, sessionHook)
	case boil.BeforeUpdateHook:
		sessionBeforeUpdateHooks = append(sessionBeforeUpdateHooks, sessionHook)
	case boil.BeforeDeleteHook:
		sessionBeforeDeleteHooks = append(sessionBeforeDeleteHooks, sessionHook)
	case boil.BeforeUpsertHook:
		sessionBeforeUpsertHooks = append(sessionBeforeUpsertHooks, sessionHook)
	case boil.AfterInsertHook:
		sessionAfterInsertHooks = append(sessionAfterInsertHooks, sessionHook)
	case boil.AfterSelectHook:
		sessionAfterSelectHooks = append(sessionAfterSelectHooks, sessionHook)
	case boil.AfterUpdateHook:
		sessionAfterUpdateHooks = append(sessionAfterUpdateHooks, sessionHook)
	case boil.AfterDeleteHook:
		sessionAfterDeleteHooks = append(sessionAfterDeleteHooks, sessionHook)
	case boil.AfterUpsertHook:
		sessionAfterUpsertHooks = append(sessionAfterUpsertHooks, sessionHook)
	}
}

// OneP returns a single session record from the query, and panics on error.
func (q sessionQuery) OneP() *Session {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single session record from the query.
func (q sessionQuery) One() (*Session, error) {
	o := &Session{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for session")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all Session records from the query, and panics on error.
func (q sessionQuery) AllP() SessionSlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all Session records from the query.
func (q sessionQuery) All() (SessionSlice, error) {
	var o SessionSlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to Session slice")
	}

	if len(sessionAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all Session records in the query, and panics on error.
func (q sessionQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all Session records in the query.
func (q sessionQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count session rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q sessionQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q sessionQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if session exists")
	}

	return count > 0, nil
}

// AccountG pointed to by the foreign key.
func (o *Session) AccountG(mods ...qm.QueryMod) accountQuery {
	return o.AccountByFk(boil.GetDB(), mods...)
}

// Account pointed to by the foreign key.
func (o *Session) AccountByFk(exec boil.Executor, mods ...qm.QueryMod) accountQuery {
	queryMods := []qm.QueryMod{
		qm.Where("id=?", o.Account),
	}

	queryMods = append(queryMods, mods...)

	query := Accounts(exec, queryMods...)
	queries.SetFrom(query.Query, "\"account\"")

	return query
}

// LoadAccount allows an eager lookup of values, cached into the
// loaded structs of the objects.
func (sessionL) LoadAccount(e boil.Executor, singular bool, maybeSession interface{}) error {
	var slice []*Session
	var object *Session

	count := 1
	if singular {
		object = maybeSession.(*Session)
	} else {
		slice = *maybeSession.(*SessionSlice)
		count = len(slice)
	}

	args := make([]interface{}, count)
	if singular {
		if object.R == nil {
			object.R = &sessionR{}
		}
		args[0] = object.Account
	} else {
		for i, obj := range slice {
			if obj.R == nil {
				obj.R = &sessionR{}
			}
			args[i] = obj.Account
		}
	}

	query := fmt.Sprintf(
		"select * from \"account\" where \"id\" in (%s)",
		strmangle.Placeholders(dialect.IndexPlaceholders, count, 1, 1),
	)

	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query, args)
	}

	results, err := e.Query(query, args...)
	if err != nil {
		return errors.Wrap(err, "failed to eager load Account")
	}
	defer results.Close()

	var resultSlice []*Account
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice Account")
	}

	if len(sessionAfterSelectHooks) != 0 {
		for _, obj := range resultSlice {
			if err := obj.doAfterSelectHooks(e); err != nil {
				return err
			}
		}
	}

	if len(resultSlice) == 0 {
		return nil
	}

	if singular {
		object.R.Account = resultSlice[0]
		return nil
	}

	for _, local := range slice {
		for _, foreign := range resultSlice {
			if local.Account == foreign.ID {
				local.R.Account = foreign
				break
			}
		}
	}

	return nil
}

// SetAccountG of the session to the related item.
// Sets o.R.Account to related.
// Adds o to related.R.Sessions.
// Uses the global database handle.
func (o *Session) SetAccountG(insert bool, related *Account) error {
	return o.SetAccount(boil.GetDB(), insert, related)
}

// SetAccountP of the session to the related item.
// Sets o.R.Account to related.
// Adds o to related.R.Sessions.
// Panics on error.
func (o *Session) SetAccountP(exec boil.Executor, insert bool, related *Account) {
	if err := o.SetAccount(exec, insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetAccountGP of the session to the related item.
// Sets o.R.Account to related.
// Adds o to related.R.Sessions.
// Uses the global database handle and panics on error.
func (o *Session) SetAccountGP(insert bool, related *Account) {
	if err := o.SetAccount(boil.GetDB(), insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetAccount of the session to the related item.
// Sets o.R.Account to related.
// Adds o to related.R.Sessions.
func (o *Session) SetAccount(exec boil.Executor, insert bool, related *Account) error {
	var err error
	if insert {
		if err = related.Insert(exec); err != nil {
			return errors.Wrap(err, "failed to insert into foreign table")
		}
	}

	updateQuery := fmt.Sprintf(
		"UPDATE \"session\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, []string{"account"}),
		strmangle.WhereClause("\"", "\"", 2, sessionPrimaryKeyColumns),
	)
	values := []interface{}{related.ID, o.Token}

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, updateQuery)
		fmt.Fprintln(boil.DebugWriter, values)
	}

	if _, err = exec.Exec(updateQuery, values...); err != nil {
		return errors.Wrap(err, "failed to update local table")
	}

	o.Account = related.ID

	if o.R == nil {
		o.R = &sessionR{
			Account: related,
		}
	} else {
		o.R.Account = related
	}

	if related.R == nil {
		related.R = &accountR{
			Sessions: SessionSlice{o},
		}
	} else {
		related.R.Sessions = append(related.R.Sessions, o)
	}

	return nil
}

// SessionsG retrieves all records.
func SessionsG(mods ...qm.QueryMod) sessionQuery {
	return Sessions(boil.GetDB(), mods...)
}

// Sessions retrieves all the records using an executor.
func Sessions(exec boil.Executor, mods ...qm.QueryMod) sessionQuery {
	mods = append(mods, qm.From("\"session\""))
	return sessionQuery{NewQuery(exec, mods...)}
}

// FindSessionG retrieves a single record by ID.
func FindSessionG(token string, selectCols ...string) (*Session, error) {
	return FindSession(boil.GetDB(), token, selectCols...)
}

// FindSessionGP retrieves a single record by ID, and panics on error.
func FindSessionGP(token string, selectCols ...string) *Session {
	retobj, err := FindSession(boil.GetDB(), token, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// FindSession retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindSession(exec boil.Executor, token string, selectCols ...string) (*Session, error) {
	sessionObj := &Session{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"session\" where \"token\"=$1", sel,
	)

	q := queries.Raw(exec, query, token)

	err := q.Bind(sessionObj)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from session")
	}

	return sessionObj, nil
}

// FindSessionP retrieves a single record by ID with an executor, and panics on error.
func FindSessionP(exec boil.Executor, token string, selectCols ...string) *Session {
	retobj, err := FindSession(exec, token, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *Session) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *Session) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *Session) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *Session) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no session provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(sessionColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	sessionInsertCacheMut.RLock()
	cache, cached := sessionInsertCache[key]
	sessionInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			sessionColumns,
			sessionColumnsWithDefault,
			sessionColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(sessionType, sessionMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(sessionType, sessionMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"session\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"session\" DEFAULT VALUES"
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
		return errors.Wrap(err, "models: unable to insert into session")
	}

	if !cached {
		sessionInsertCacheMut.Lock()
		sessionInsertCache[key] = cache
		sessionInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}

// UpdateG a single Session record. See Update for
// whitelist behavior description.
func (o *Session) UpdateG(whitelist ...string) error {
	return o.Update(boil.GetDB(), whitelist...)
}

// UpdateGP a single Session record.
// UpdateGP takes a whitelist of column names that should be updated.
// Panics on error. See Update for whitelist behavior description.
func (o *Session) UpdateGP(whitelist ...string) {
	if err := o.Update(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateP uses an executor to update the Session, and panics on error.
// See Update for whitelist behavior description.
func (o *Session) UpdateP(exec boil.Executor, whitelist ...string) {
	err := o.Update(exec, whitelist...)
	if err != nil {
		panic(boil.WrapErr(err))
	}
}

// Update uses an executor to update the Session.
// Whitelist behavior: If a whitelist is provided, only the columns given are updated.
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns are inferred to start with
// - All primary keys are subtracted from this set
// Update does not automatically update the record in case of default values. Use .Reload()
// to refresh the records.
func (o *Session) Update(exec boil.Executor, whitelist ...string) error {
	var err error
	if err = o.doBeforeUpdateHooks(exec); err != nil {
		return err
	}
	key := makeCacheKey(whitelist, nil)
	sessionUpdateCacheMut.RLock()
	cache, cached := sessionUpdateCache[key]
	sessionUpdateCacheMut.RUnlock()

	if !cached {
		wl := strmangle.UpdateColumnSet(sessionColumns, sessionPrimaryKeyColumns, whitelist)
		if len(whitelist) == 0 {
			wl = strmangle.SetComplement(wl, []string{"created_at"})
		}
		if len(wl) == 0 {
			return errors.New("models: unable to update session, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"session\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, sessionPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(sessionType, sessionMapping, append(wl, sessionPrimaryKeyColumns...))
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
		return errors.Wrap(err, "models: unable to update session row")
	}

	if !cached {
		sessionUpdateCacheMut.Lock()
		sessionUpdateCache[key] = cache
		sessionUpdateCacheMut.Unlock()
	}

	return o.doAfterUpdateHooks(exec)
}

// UpdateAllP updates all rows with matching column names, and panics on error.
func (q sessionQuery) UpdateAllP(cols M) {
	if err := q.UpdateAll(cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values.
func (q sessionQuery) UpdateAll(cols M) error {
	queries.SetUpdate(q.Query, cols)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to update all for session")
	}

	return nil
}

// UpdateAllG updates all rows with the specified column values.
func (o SessionSlice) UpdateAllG(cols M) error {
	return o.UpdateAll(boil.GetDB(), cols)
}

// UpdateAllGP updates all rows with the specified column values, and panics on error.
func (o SessionSlice) UpdateAllGP(cols M) {
	if err := o.UpdateAll(boil.GetDB(), cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAllP updates all rows with the specified column values, and panics on error.
func (o SessionSlice) UpdateAllP(exec boil.Executor, cols M) {
	if err := o.UpdateAll(exec, cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o SessionSlice) UpdateAll(exec boil.Executor, cols M) error {
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
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), sessionPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf(
		"UPDATE \"session\" SET %s WHERE (\"token\") IN (%s)",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(sessionPrimaryKeyColumns), len(colNames)+1, len(sessionPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, sql)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(sql, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update all in session slice")
	}

	return nil
}

// UpsertG attempts an insert, and does an update or ignore on conflict.
func (o *Session) UpsertG(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	return o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...)
}

// UpsertGP attempts an insert, and does an update or ignore on conflict. Panics on error.
func (o *Session) UpsertGP(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpsertP attempts an insert using an executor, and does an update or ignore on conflict.
// UpsertP panics on error.
func (o *Session) UpsertP(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(exec, updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
func (o *Session) Upsert(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no session provided for upsert")
	}

	if err := o.doBeforeUpsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(sessionColumnsWithDefault, o)

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

	sessionUpsertCacheMut.RLock()
	cache, cached := sessionUpsertCache[key]
	sessionUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		var ret []string
		whitelist, ret = strmangle.InsertColumnSet(
			sessionColumns,
			sessionColumnsWithDefault,
			sessionColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)
		update := strmangle.UpdateColumnSet(
			sessionColumns,
			sessionPrimaryKeyColumns,
			updateColumns,
		)
		if len(update) == 0 {
			return errors.New("models: unable to upsert session, could not build update column list")
		}

		conflict := conflictColumns
		if len(conflict) == 0 {
			conflict = make([]string, len(sessionPrimaryKeyColumns))
			copy(conflict, sessionPrimaryKeyColumns)
		}
		cache.query = queries.BuildUpsertQueryPostgres(dialect, "\"session\"", updateOnConflict, ret, update, conflict, whitelist)

		cache.valueMapping, err = queries.BindMapping(sessionType, sessionMapping, whitelist)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(sessionType, sessionMapping, ret)
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
		return errors.Wrap(err, "models: unable to upsert session")
	}

	if !cached {
		sessionUpsertCacheMut.Lock()
		sessionUpsertCache[key] = cache
		sessionUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(exec)
}

// DeleteP deletes a single Session record with an executor.
// DeleteP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *Session) DeleteP(exec boil.Executor) {
	if err := o.Delete(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteG deletes a single Session record.
// DeleteG will match against the primary key column to find the record to delete.
func (o *Session) DeleteG() error {
	if o == nil {
		return errors.New("models: no Session provided for deletion")
	}

	return o.Delete(boil.GetDB())
}

// DeleteGP deletes a single Session record.
// DeleteGP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *Session) DeleteGP() {
	if err := o.DeleteG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Delete deletes a single Session record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *Session) Delete(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no Session provided for delete")
	}

	if err := o.doBeforeDeleteHooks(exec); err != nil {
		return err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), sessionPrimaryKeyMapping)
	sql := "DELETE FROM \"session\" WHERE \"token\"=$1"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, sql)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(sql, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete from session")
	}

	if err := o.doAfterDeleteHooks(exec); err != nil {
		return err
	}

	return nil
}

// DeleteAllP deletes all rows, and panics on error.
func (q sessionQuery) DeleteAllP() {
	if err := q.DeleteAll(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all matching rows.
func (q sessionQuery) DeleteAll() error {
	if q.Query == nil {
		return errors.New("models: no sessionQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from session")
	}

	return nil
}

// DeleteAllGP deletes all rows in the slice, and panics on error.
func (o SessionSlice) DeleteAllGP() {
	if err := o.DeleteAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAllG deletes all rows in the slice.
func (o SessionSlice) DeleteAllG() error {
	if o == nil {
		return errors.New("models: no Session slice provided for delete all")
	}
	return o.DeleteAll(boil.GetDB())
}

// DeleteAllP deletes all rows in the slice, using an executor, and panics on error.
func (o SessionSlice) DeleteAllP(exec boil.Executor) {
	if err := o.DeleteAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o SessionSlice) DeleteAll(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no Session slice provided for delete all")
	}

	if len(o) == 0 {
		return nil
	}

	if len(sessionBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), sessionPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf(
		"DELETE FROM \"session\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, sessionPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(sessionPrimaryKeyColumns), 1, len(sessionPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, sql)
		fmt.Fprintln(boil.DebugWriter, args)
	}

	_, err := exec.Exec(sql, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from session slice")
	}

	if len(sessionAfterDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	return nil
}

// ReloadGP refetches the object from the database and panics on error.
func (o *Session) ReloadGP() {
	if err := o.ReloadG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadP refetches the object from the database with an executor. Panics on error.
func (o *Session) ReloadP(exec boil.Executor) {
	if err := o.Reload(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadG refetches the object from the database using the primary keys.
func (o *Session) ReloadG() error {
	if o == nil {
		return errors.New("models: no Session provided for reload")
	}

	return o.Reload(boil.GetDB())
}

// Reload refetches the object from the database
// using the primary keys with an executor.
func (o *Session) Reload(exec boil.Executor) error {
	ret, err := FindSession(exec, o.Token)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAllGP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *SessionSlice) ReloadAllGP() {
	if err := o.ReloadAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *SessionSlice) ReloadAllP(exec boil.Executor) {
	if err := o.ReloadAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllG refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *SessionSlice) ReloadAllG() error {
	if o == nil {
		return errors.New("models: empty SessionSlice provided for reload all")
	}

	return o.ReloadAll(boil.GetDB())
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *SessionSlice) ReloadAll(exec boil.Executor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	sessions := SessionSlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), sessionPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf(
		"SELECT \"session\".* FROM \"session\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, sessionPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(*o)*len(sessionPrimaryKeyColumns), 1, len(sessionPrimaryKeyColumns)),
	)

	q := queries.Raw(exec, sql, args...)

	err := q.Bind(&sessions)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in SessionSlice")
	}

	*o = sessions

	return nil
}

// SessionExists checks if the Session row exists.
func SessionExists(exec boil.Executor, token string) (bool, error) {
	var exists bool

	sql := "select exists(select 1 from \"session\" where \"token\"=$1 limit 1)"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, sql)
		fmt.Fprintln(boil.DebugWriter, token)
	}

	row := exec.QueryRow(sql, token)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if session exists")
	}

	return exists, nil
}

// SessionExistsG checks if the Session row exists.
func SessionExistsG(token string) (bool, error) {
	return SessionExists(boil.GetDB(), token)
}

// SessionExistsGP checks if the Session row exists. Panics on error.
func SessionExistsGP(token string) bool {
	e, err := SessionExists(boil.GetDB(), token)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// SessionExistsP checks if the Session row exists. Panics on error.
func SessionExistsP(exec boil.Executor, token string) bool {
	e, err := SessionExists(exec, token)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}
