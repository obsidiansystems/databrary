package tests

import (
	"reflect"
	"testing"

	. "github.com/databrary/databrary/db/models/custom_types"
)

func TestPermission(t *testing.T) {
	testFuncs = []testFunc{
		{"", testPermission},
	}
	test(t)
}

func testPermission(t *testing.T) {
	perm := Permission{}

	// Test scanning NULL values
	rows, err := testConn.QueryRow("SELECT NULL::audit.action")
	if err != nil {
		t.Fatalf("db error %s", err)
	}
	rows.Scan(&perm)
	if perm.Valid {
		t.Fatal("expected null result")
	}
	//
	//// Test setting NULL values
	rows, err = testConn.QueryRow("SELECT $1::permission", perm)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	rows.Scan(&perm)
	if perm.Valid {
		t.Fatalf("expected null result")
	}

	// test encoding in query params, then decoding during Scan
	testBidirectional := func(p Permission) {
		rows, err := testConn.QueryRow("SELECT $1::permission", p)
		if err != nil {
			t.Fatalf("re-query %s permission failed: %s", err.Error())
		}
		rows.Scan(&perm)
		if !perm.Valid {
			t.Fatalf("expected non-null value, got null for %#v", perm)
		}
		if !reflect.DeepEqual(p, perm) {
			t.Fatalf("expected permission to match, but did not for \n%#v\n%#v", p, perm)
		}
	}

	testBidirectional(PermADMIN)
	testBidirectional(PermSHARED)
	testBidirectional(PermPUBLIC)
	testBidirectional(PermNONE)
	testBidirectional(PermEDIT)
	testBidirectional(PermREAD)

}
