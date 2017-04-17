package tests

import (
	db "github.com/databrary/databrary/db"
	"github.com/spf13/viper"
	"testing"
	"upper.io/db.v3/lib/sqlbuilder"
)

func OpenTestConn(conf *viper.Viper, t *testing.T) sqlbuilder.Database {

	if conn, err := db.OpenConn(conf); err != nil {
		t.Fatal(err)
		return nil
	} else {
		return conn
	}

}
