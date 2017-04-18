package tests

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/databrary/databrary/config"
	. "github.com/databrary/databrary/db/models/custom_types/varchar"
	"github.com/databrary/databrary/logging"
)

func TestVarChar(t *testing.T) {
	config.InitConf("../../../config/databrary_test.toml")
	conf := config.GetConf()
	logging.InitLgr(conf)
	conn := OpenTestConn(conf, t)
	defer conn.Close()

	n := VarChar{}

	// Test scanning NULL values
	rows, err := conn.QueryRow("SELECT NULL::varchar(10)")
	if err != nil {
		t.Fatalf("db error %s", err)
	}
	rows.Scan(&n)
	if n.Valid() {
		t.Fatalf("expected null result")
	}

	// Test setting NULL values
	rows, err = conn.QueryRow("SELECT $1::varchar(10)", n)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	rows.Scan(&n)
	if n.Valid() {
		t.Fatalf("expected null result")
	}

	// test encoding in query params, then decoding during Scan
	testBidirectional := func(v VarChar, charLength int) {
		rows, err = conn.QueryRow(fmt.Sprintf("SELECT $1::varchar(%d)", charLength), v)
		if err != nil {
			t.Fatalf("re-query failed: %s", err.Error())
		}
		rows.Scan(&n)
		if !n.Valid() {
			t.Fatalf("expected non-null value, got null for %#v", n)
		}
		if !reflect.DeepEqual(n, v) {
			t.Fatalf("expected strings to match, but did not for\n%#v\n%#v", v, n)
		}
	}

	testBidirectional(NewVarChar("abcdef", true), 10)   //intentionally longer
	testBidirectional(NewVarChar("abcdef  ", true), 10) //intentionally longer

}
