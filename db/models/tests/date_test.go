package tests

import (
	"reflect"
	"testing"
	"time"

	"github.com/databrary/databrary/config"
	. "github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/databrary/logging"
)

func TestDate(t *testing.T) {
	config.InitConf("../../../config/databrary_test.toml")
	conf := config.GetConf()
	logging.InitLgr(conf)
	conn := OpenTestConn(conf, t)
	defer conn.Close()

	n := Date{}

	// Test scanning NULL values
	rows, err := conn.QueryRow("SELECT NULL::date")
	if err != nil {
		t.Fatalf("db error %s", err)
	}
	rows.Scan(&n)
	if n.Valid() {
		t.Fatalf("expected null result")
	}

	// Test setting NULL values
	rows, err = conn.QueryRow("SELECT $1::date", n)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	rows.Scan(&n)
	if n.Valid() {
		t.Fatalf("expected null result")
	}

	// test encoding in query params, then decoding during Scan
	testBidirectional := func(v Date) {
		rows, err = conn.QueryRow("SELECT $1::date", v)
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

	testBidirectional(NewDate(time.Now(), true)) //intentionally longer
}
