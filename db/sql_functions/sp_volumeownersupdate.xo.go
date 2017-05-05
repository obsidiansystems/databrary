// Package xo_models contains the types for schema 'public'.
package xo_models

// GENERATED BY XO. DO NOT EDIT.

import (
	"fmt"

	"github.com/vattle/sqlboiler/boil"
)

// VolumeOwnersUpdate calls the stored procedure 'public.volume_owners_update(integer) void' on db.
func VolumeOwnersUpdate(exec boil.Executor, v0 int) error {
	var err error

	// sql query
	const query = `SELECT public.volume_owners_update($1)`

	// run query
	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query, v0)
	}
	_, err = exec.Exec(query)
	return err
}