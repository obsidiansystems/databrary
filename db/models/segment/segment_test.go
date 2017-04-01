package segment

import (
	"testing"

	"github.com/SaidinWoT/timespan"
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/logging"
	"time"
	"upper.io/db.v3/lib/sqlbuilder"
	pg "upper.io/db.v3/postgresql"
)

func init() {
	config.InitConf("../../../config/databrary_dev.toml")
	logging.InitLgr(config.GetConf())
}

type Fatalistic interface {
	Fatal(args ...interface{})
}

func openTestConn(t Fatalistic) sqlbuilder.Database {

	conf := config.GetConf()
	settings := &pg.ConnectionURL{
		Host:     conf.GetString("database.addr") + ":" + conf.GetString("database.port"),
		Database: conf.GetString("database.db_name"),
		User:     conf.GetString("database.user"),
		Password: conf.GetString("database.pw"),
	}

	conn, err := pg.Open(settings)
	if err != nil {
		t.Fatal(err)
	}

	return conn
}

func TestInet(t *testing.T) {
	conn := openTestConn(t)
	defer conn.Close()

	segment := Segment{}

	//// Test scanning NULL values
	rows, err := conn.QueryRow("SELECT NULL::segment")
	if err != nil {
		t.Fatalf("db error %s", err)
	}
	rows.Scan(&segment)
	if segment.Valid {
		t.Fatalf("expected null result")
	}

	// Test setting NULL values
	rows, err = conn.QueryRow("SELECT $1::segment", segment)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	rows.Scan(&segment)
	if segment.Valid {
		t.Fatalf("expected null result")
	}

	//test encoding in query params, then decoding during Scan
	testBidirectional := func(s Segment, label string) {
		rows, err = conn.QueryRow("SELECT $1::segment", s)
		if err != nil {
			t.Fatalf("re-query %s segment failed: %s", label, err.Error())
		}
		rows.Scan(&segment)
		if !segment.Valid {
			t.Fatalf("expected non-null value, got null for %s", label)
		}

		if segment.Segment.Equal(*s.Segment) != true {
			t.Fatalf("expected segments to match, but did not for %s - \n%s \n%s\n\n%#v \n%#v", label, s.String(), segment.String(), s.Segment, segment.Segment)
		}
	}

	// postgres truncates "00:00:00.000" to "00:00:00" so need extra milli
	zeroT, _ := time.Parse(MIL_SEG_FORMAT, "00:00:00.000")
	d, _ := time.ParseDuration("2h45m0s0us")
	ts := timespan.New(zeroT, d)
	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple time milli")

	zeroT, _ = time.Parse(SEG_FORMAT, "00:00:00")
	d, _ = time.ParseDuration("2h45m0s0us")
	ts = timespan.New(zeroT, d)
	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple time no milli")

	zeroT, _ = time.Parse(SEG_FORMAT, "00:00:05")
	d, _ = time.ParseDuration("2h45m")
	ts = timespan.New(zeroT, d)
	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple time no milli")

	// Test setting mixed values
	zeroT, _ = time.Parse(SEG_FORMAT, "00:18:34")
	endT, _ := time.Parse(SEG_FORMAT, "00:22:46.99")
	d = endT.Sub(zeroT)
	ts = timespan.New(zeroT, d)
	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple mixed")

	// Test setting mixed values
	zeroT, _ = time.Parse(SEG_FORMAT, "00:18:34.1")
	endT, _ = time.Parse(SEG_FORMAT, "00:22:46")
	d = endT.Sub(zeroT)
	ts = timespan.New(zeroT, d)
	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple mixed")

	// Test empty
	d = endT.Sub(endT)
	ts = timespan.New(zeroT, d)
	rows, err = conn.QueryRow("SELECT $1::segment", Segment{Segment: &ts, Valid: true})
	err = rows.Scan(&segment)
	if segment.Valid || err == nil {
		t.Fatalf("expected err and invalid segment")
	}

	rows, err = conn.QueryRow("SELECT '(,)'::segment")
	err = rows.Scan(&segment)
	if !(segment.Valid && segment.Segment == nil) {
		t.Fatalf("expected (,)")
	}

}
