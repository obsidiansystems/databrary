package tests

import (
	"reflect"
	"testing"

	. "github.com/databrary/databrary/db/models/custom_types"
)

func TestReleaseLvl(t *testing.T) {
	testFuncs = []testFunc{
		{"", testReleaseLvl},
	}
	test(t)
}

func testReleaseLvl(t *testing.T) {
	act := ReleaseLvl{}

	// Test scanning NULL values
	rows, err := testConn.QueryRow("SELECT NULL::release")
	if err != nil {
		t.Fatalf("db error %s", err)
	}
	rows.Scan(&act)
	if act.Valid {
		t.Fatal("expected null result")
	}
	//
	//// Test setting NULL values
	rows, err = testConn.QueryRow("SELECT $1::release", act)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	rows.Scan(&act)
	if act.Valid {
		t.Fatalf("expected null result")
	}

	// test encoding in query params, then decoding during Scan
	testBidirectional := func(a ReleaseLvl) {
		rows, err := testConn.QueryRow("SELECT $1::release", a)
		if err != nil {
			t.Fatalf("re-query %s action failed: %s", err.Error())
		}
		rows.Scan(&act)
		if !act.Valid {
			t.Fatalf("expected non-null value, got null for %#v", act)
		}
		if !reflect.DeepEqual(a, act) {
			t.Fatalf("expected actions to match, but did not for \n%#v\n%#v", a, act)
		}
	}

	testBidirectional(ReleaseLvlEXCERPTS)
	testBidirectional(ReleaseLvlPRIVATE)
	testBidirectional(ReleaseLvlPUBLIC)
	testBidirectional(ReleaseLvlSHARED)

}
