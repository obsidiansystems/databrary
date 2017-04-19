package tests

import (
	"reflect"
	"testing"

	. "github.com/databrary/databrary/db/models/custom_types"
)

func TestAction(t *testing.T) {
	testFuncs = []testFunc{
		{"", testAction},
	}
	test(t)
}

func testAction(t *testing.T) {
	act := Action{}

	// Test scanning NULL values
	rows, err := testConn.QueryRow("SELECT NULL::audit.action")
	if err != nil {
		t.Fatalf("db error %s", err)
	}
	rows.Scan(&act)
	if act.Valid {
		t.Fatal("expected null result")
	}
	//
	//// Test setting NULL values
	rows, err = testConn.QueryRow("SELECT $1::audit.action", act)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	rows.Scan(&act)
	if act.Valid {
		t.Fatalf("expected null result")
	}

	// test encoding in query params, then decoding during Scan
	testBidirectional := func(a Action) {
		rows, err := testConn.QueryRow("SELECT $1::audit.action", a)
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

	testBidirectional(ActionADD)
	testBidirectional(ActionATTEMPT)
	testBidirectional(ActionCHANGE)
	testBidirectional(ActionCLOSE)
	testBidirectional(ActionOPEN)
	testBidirectional(ActionREMOVE)
	testBidirectional(ActionSUPERUSER)

}
