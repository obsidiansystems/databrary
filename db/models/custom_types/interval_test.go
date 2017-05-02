package custom_types

import (
	"testing"

	"fmt"
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	"github.com/jmoiron/sqlx"
	"os"
	"path/filepath"
	"time"
)

var connInterval *sqlx.DB

func TestInterval(t *testing.T) {
	GOPATH := os.Getenv("GOPATH")
	config.InitConf(filepath.Join(GOPATH, "src/github.com/databrary/databrary/config/databrary_test.toml"))
	conf := config.GetConf()
	var err error
	connInterval, err = db.OpenConn(conf)
	if err != nil {
		t.Fatal("failed to open db connInet")
	}
	defer connInterval.Close()
	t.Run("non null", testIntervalNonNull)
	t.Run("null", testNullInterval)
	t.Run("EQ", testInterval_EQ)
	t.Run("LT", testInterval_LT)
	t.Run("LE", testInterval_LE)
}

func testIntervalNonNull(t *testing.T) {

	inter := Interval{}

	// test encoding in query params, then decoding during Scan
	testBidirectional := func(label string, i Interval) {
		rows, err := connInterval.Query("SELECT $1::interval HOUR TO SECOND (3)", i)
		if err != nil {
			t.Errorf("re-query %s failed: %s", label, err.Error())
		}
		if rows.Next() {
			err = rows.Scan(&inter)
			if err != nil {
				t.Fatal(err)
			}
			if i.interval != inter.interval {
				t.Fatalf("expected intervals %s to match, but did not for %#v %#v", label, inter, i)
			}
		} else {
			t.Errorf("no results for %s", label)
		}
	}
	d, _ := time.ParseDuration(".001s")
	testBidirectional(".001s", NewInterval(d))
	d, _ = time.ParseDuration("-.001s")
	testBidirectional("-.001s", NewInterval(d))
	d, _ = time.ParseDuration("-.00101s")
	testBidirectional("-.00101s", NewInterval(d))
	d, _ = time.ParseDuration("1.001s")
	testBidirectional("1.001s", NewInterval(d))
	d, _ = time.ParseDuration("-1.001s")
	testBidirectional("-1.001s", NewInterval(d))
	d, _ = time.ParseDuration("-1.0005s")
	testBidirectional("-1.0005s", NewInterval(d))
	d, _ = time.ParseDuration("1.0005s")
	testBidirectional("1.0005s", NewInterval(d))

	i := Interval{}
	i.Scan([]byte("16:00:00.216"))
	fmt.Println(i)
}

func testNullInterval(t *testing.T) {
	inter := NullInterval{}
	// Test scanning NULL values
	rows, err := connInterval.Query("SELECT NULL::interval HOUR TO SECOND (3)")
	if err != nil {
		t.Fatal(err)
	}
	if rows.Next() {
		err = rows.Scan(&inter)
		if err != nil {
			t.Fatalf("db error %+v", err)
		}
	} else {
		t.Fatal("no results for scan null interval")
	}

	// Test setting NULL values
	rows, err = connInterval.Query("SELECT $1::inet", inter)
	if err != nil {
		t.Fatal(err)
	}
	if rows.Next() {
		err = rows.Scan(&inter)
		if err != nil {
			t.Fatalf("db error %s", err)
		}
	} else {
		t.Fatal("no results for Value null interval")
	}
}

func testInterval_EQ(t *testing.T) {
	d, _ := time.ParseDuration(".001s")
	d1, _ := time.ParseDuration(".001s")
	n, n1 := NewInterval(d), NewInterval(d1)
	if !n.EQ(n1) {
		t.Errorf("%#v %#v should be equal", n, n1)
	}
	d, _ = time.ParseDuration(".003s")
	d1, _ = time.ParseDuration("-.001s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if n.EQ(n1) {
		t.Errorf("%#v %#v should not be equal", n, n1)
	}

}

func testInterval_LT(t *testing.T) {
	d, _ := time.ParseDuration(".001s")
	d1, _ := time.ParseDuration(".011s")
	n, n1 := NewInterval(d), NewInterval(d1)
	if !n.LT(n1) {
		t.Errorf("%#v %#v should be LT", n, n1)
	}
	d, _ = time.ParseDuration(".003s")
	d1, _ = time.ParseDuration("-.001s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if n.LT(n1) {
		t.Errorf("%#v %#v should not be LT", n, n1)
	}
	if !n1.LT(n) {
		t.Fatal("%#v %#v should be LT", n1, n)
	}
}

func testInterval_LE(t *testing.T) {
	d, _ := time.ParseDuration(".001s")
	d1, _ := time.ParseDuration(".011s")
	n, n1 := NewInterval(d), NewInterval(d1)
	if !n.LE(n1) {
		t.Errorf("%#v %#v should be LE", n, n1)
	}
	d, _ = time.ParseDuration(".003s")
	d1, _ = time.ParseDuration("-.001s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if n.LE(n1) {
		t.Errorf("%#v %#v should not be LE", n, n1)
	}
	d1, _ = time.ParseDuration(".001s")
	n1 = NewInterval(d1)
	if !n1.LE(n) {
		t.Errorf("%#v %#v should be LE", n1, n)
	}
	d, _ = time.ParseDuration(".0003s")
	d1, _ = time.ParseDuration("-.0001s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if !n.LE(n1) {
		t.Errorf("%#v %#v should be LE", n, n1)
	}
}
