// this is until https://github.com/lib/pq/pull/390 gets merged in

package custom_types

import (
	"bytes"
	"net"
	"testing"

	"os"
	"path/filepath"

	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	"github.com/jmoiron/sqlx"
)

var connInet *sqlx.DB

func TestInet(t *testing.T) {
	var err error
	GOPATH := os.Getenv("GOPATH")
	config.InitConf(filepath.Join(GOPATH, "src/github.com/databrary/databrary/config/databrary_test.toml"))
	conf := config.GetConf()
	// initialize db connection
	err = db.InitDB(conf)
	if err != nil {
		panic(err.Error())
	}
	defer connInet.Close()

	t.Run("non null", testNonNullInet)
	t.Run("null", testNullInet)

}

func testNonNullInet(t *testing.T) {
	inet := Inet{}

	// test encoding in query params, then decoding during Scan
	testBidirectional := func(i Inet, label string) {
		rows, err := connInet.Query("SELECT $1::inet", i)
		if err != nil {
			t.Fatalf("%s: db error %s", label, err)
		}
		if rows.Next() {
			err = rows.Scan(&inet)
			if err != nil {
				t.Error(err)
			}
			if bytes.Compare(i, inet) != 0 {
				t.Errorf("expected IP addresses to match, but did not for %s - %#v %#v", label, i, inet)
			}
		} else {
			t.Errorf("no results for %s", label)
		}
	}
	testBidirectional(Inet(net.ParseIP("192.168.0.1")), "Simple IPv4")
	testBidirectional(Inet(net.ParseIP("::1")), "Loopback IPv6")
	testBidirectional(Inet(net.ParseIP("abcd:2345::")), "Loopback IPv6")

	//Bad argument
	inet = Inet{}
	err := inet.Scan(456)
	if err == nil {
		t.Fatal("Expected error for non-byte[] argument to Scan")
	}
}

func testNullInet(t *testing.T) {
	inet := NullInet{}
	// Test scanning NULL values
	rows, err := connInet.Query("SELECT NULL::inet")
	if err != nil {
		t.Fatalf("scan null inet: db error %s", err)
	}
	if rows.Next() {
		rows.Scan(&inet)
		if inet.Valid {
			t.Fatalf("expected null result")
		}
	} else {
		t.Fatal("no results for scan null inet")
	}
	// Test setting NULL values
	rows, err = connInet.Query("SELECT $1::inet", inet)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	if rows.Next() {
		rows.Scan(&inet)
		if inet.Valid {
			t.Fatalf("expected null result")
		}
	} else {
		t.Fatal("no results for Value null inet")
	}
}
