package custom_types

import (
	"testing"

	"os"
	"path/filepath"
	"time"

	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	"github.com/jmoiron/sqlx"
)

var connInterval *sqlx.DB

func TestInterval(t *testing.T) {
	GOPATH := os.Getenv("GOPATH")
	config.InitConf(filepath.Join(GOPATH, "src/github.com/databrary/databrary/config/databrary_test.toml"))
	conf := config.GetConf()
	var err error
	// initialize db connection
	err = db.InitDB(conf)
	if err != nil {
		panic(err.Error())
	}
	defer connInterval.Close()
	t.Run("non null", testIntervalNonNull)
	t.Run("null", testNullInterval)
	t.Run("EQ", testInterval_EQ)
	t.Run("GT", testInterval_GT)
	t.Run("GE", testInterval_GE)
	t.Run("LT", testInterval_LT)
	t.Run("LE", testInterval_LE)
	t.Run("new from strong", testNewIntervalFromString)
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
				t.Error(err)
			}
			if i.interval != inter.interval {
				t.Errorf("expected intervals %s to match, but did not for %#v %#v", label, inter, i)
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
}

func testNewIntervalFromString(t *testing.T) {
	i := NewIntervalFromString("16:00:00.216")
	d, _ := time.ParseDuration("16h.216s")
	j := NewInterval(d)
	if !i.EQ(j) {
		t.Error("should be equal")
	}
}

func testNullInterval(t *testing.T) {
	inter := NullInterval{}
	// Test scanning NULL values
	rows, err := connInterval.Query("SELECT NULL::interval HOUR TO SECOND (3)")
	if err != nil {
		t.Error(err)
	}
	if rows.Next() {
		err = rows.Scan(&inter)
		if err != nil {
			t.Errorf("db error %+v", err)
		}
	} else {
		t.Error("no results for scan null interval")
	}

	// Test setting NULL values
	rows, err = connInterval.Query("SELECT $1::inet", inter)
	if err != nil {
		t.Error(err)
	}
	if rows.Next() {
		err = rows.Scan(&inter)
		if err != nil {
			t.Errorf("db error %s", err)
		}
	} else {
		t.Error("no results for Value null interval")
	}
}

func testInterval_EQ(t *testing.T) {
	d, _ := time.ParseDuration(".001s")
	d1, _ := time.ParseDuration(".001s")
	n, n1 := NewInterval(d), NewInterval(d1)
	if !n.EQ(n1) {
		t.Errorf("%#v %#v should be equal", n, n1)
	}

	d, _ = time.ParseDuration("-.0001s")
	d1, _ = time.ParseDuration("-.0006s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if n.EQ(n1) {
		t.Errorf("%#v %#v should not be equal", n, n1)
	}

	d, _ = time.ParseDuration("-.0005s")
	d1, _ = time.ParseDuration("-.0006s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if !n.EQ(n1) {
		t.Errorf("%#v %#v should be equal", n, n1)
	}

	d, _ = time.ParseDuration("-.0001s")
	d1, _ = time.ParseDuration("-.0002s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if !n.EQ(n1) {
		t.Errorf("%#v %#v should be equal", n, n1)
	}

	d, _ = time.ParseDuration("-.001s")
	d1, _ = time.ParseDuration("-.001s")
	n, n1 = NewInterval(d), NewInterval(d1)
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
		t.Error("%#v %#v should be LT", n1, n)
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

func testInterval_GT(t *testing.T) {
	d, _ := time.ParseDuration(".001s")
	d1, _ := time.ParseDuration(".011s")
	n, n1 := NewInterval(d), NewInterval(d1)
	if !n1.GT(n) {
		t.Errorf("%#v %#v should be GT", n1, n)
	}
	d, _ = time.ParseDuration(".003s")
	d1, _ = time.ParseDuration("-.001s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if n1.GT(n) {
		t.Errorf("%#v %#v should not be GT", n1, n)
	}
	if !n.GT(n1) {
		t.Error("%#v %#v should be GT", n, n1)
	}
}

func testInterval_GE(t *testing.T) {
	d, _ := time.ParseDuration(".001s")
	d1, _ := time.ParseDuration(".011s")
	n, n1 := NewInterval(d), NewInterval(d1)
	if !n1.GE(n) {
		t.Errorf("%#v %#v should be GE", n1, n)
	}
	d, _ = time.ParseDuration(".003s")
	d1, _ = time.ParseDuration("-.001s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if n1.GE(n) {
		t.Errorf("%#v %#v should not be GE", n1, n)
	}
	d1, _ = time.ParseDuration(".001s")
	n1 = NewInterval(d1)
	if !n.GE(n1) {
		t.Errorf("%#v %#v should be GE", n, n1)
	}
	d, _ = time.ParseDuration(".0003s")
	d1, _ = time.ParseDuration("-.0001s")
	n, n1 = NewInterval(d), NewInterval(d1)
	if !n1.GE(n) {
		t.Errorf("%#v %#v should be GE", n1, n)
	}
}
