package segment

import (
	"testing"

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


var times = []time.Time{
	time.Date(2014, time.February, 3, 2, 0, 0, 0, time.UTC),
	time.Date(2014, time.February, 3, 4, 0, 0, 0, time.UTC),
	time.Date(2014, time.February, 3, 6, 0, 0, 0, time.UTC),
	time.Date(2014, time.February, 3, 8, 0, 0, 0, time.UTC),
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


func TestConstructor(t *testing.T) {
	// test empty
	if _, e := NewSegment(nil, nil, ""); e != nil {
		t.Fatalf(e.Error())
	}
	// test empty erroring
	if _, e := NewSegment(&times[0], nil, ""); e == nil {
		t.Fatal("empty bounds with non-nil lower")
	}
	if _, e := NewSegment(nil, &times[0], ""); e == nil {
		t.Fatal("empty bounds with non-nil upper")
	}
	if _, e := NewSegment(&times[0], &times[1], ""); e == nil {
		t.Fatal("empty bounds with non-nil lower and upper")
	}

	// test finite
	if _, e := NewSegment(&times[0], &times[1], "[]"); e != nil {
		t.Fatal(e.Error())
	}
	if _, e := NewSegment(&times[0], &times[1], "[)"); e != nil {
		t.Fatal(e.Error())
	}
	if _, e := NewSegment(&times[0], &times[1], "(]"); e != nil {
		t.Fatal(e.Error())
	}
	if _, e := NewSegment(&times[0], &times[1], "()"); e != nil {
		t.Fatal(e.Error())
	}
	// test finite errors
	if _, e := NewSegment(&times[1], &times[0], "[]"); e == nil {
		t.Fatal(e.Error())
	}

	// test lower unbound
	if _, e := NewSegment(nil, &times[0], "(]"); e != nil {
		t.Fatal(e.Error())
	}
	if _, e := NewSegment(nil, &times[0], "()"); e != nil {
		t.Fatal(e.Error())
	}
	if _, e := NewSegment(nil, nil, "()"); e != nil {
		t.Fatal(e.Error())
	}
	// test lower unbound error
	if _, e := NewSegment(nil, &times[0], "[]"); e == nil {
		t.Fatal(e.Error())
	}
	if _, e := NewSegment(nil, nil, "[]"); e == nil {
		t.Fatal(e.Error())
	}

	// test upper unbound
	if _, e := NewSegment(&times[0], nil, "[)"); e != nil {
		t.Fatal(e.Error())
	}
	// test upper unbound error
	if _, e := NewSegment(&times[0], nil, "[]"); e == nil {
		t.Fatal(e.Error())
	}
}

func TestOrder(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if lt := s.LowerLT(t); !lt {
		tt.Fatal("should be <")
	}
	if lt := s.LowerLE(t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.LowerGT(t); lt {
		tt.Fatal("should not be >")
	}
	if lt := s.LowerGE(t); lt {
		tt.Fatal("should not be >=")
	}
	if lt := s.UpperLT(t); !lt {
		tt.Fatal("should be <")
	}
	if lt := s.UpperLE(t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.UpperGT(t); lt {
		tt.Fatal("should not be >")
	}
	if lt := s.UpperGE(t); lt {
		tt.Fatal("should not be >=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")
	if lt := s.LowerLT(t); lt {
		tt.Fatal("should not be <")
	}
	if lt := s.LowerLE(t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.LowerGT(t); lt {
		tt.Fatal("should not be >")
	}
	if lt := s.LowerGE(t); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.LowerLT(s); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.LowerLE(s); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.UpperLT(s); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.UpperLE(s); !lt {
		tt.Fatal("should be <=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.UpperLT(t); lt {
		tt.Fatal("should not be <")
	}
	if lt := s.UpperLE(t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.UpperGT(t); lt {
		tt.Fatal("should not be >=")
	}
	if lt := s.UpperGE(t); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.UpperLT(s); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.UpperLE(s); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.UpperGT(s); lt {
		tt.Fatal("should not be >")
	}
	if lt := t.UpperGE(s); !lt {
		tt.Fatal("should be >=")
	}

	// compare s with itself
	if lt := s.LowerLT(s); lt {
		tt.Fatal("should not be <")
	}
	if lt := s.LowerLE(s); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.LowerGT(s); lt {
		tt.Fatal("should not be >")
	}
	if lt := s.LowerGE(s); !lt {
		tt.Fatal("should be >=")
	}
	if lt := s.UpperLT(s); lt {
		tt.Fatal("should not be <")
	}
	if lt := s.UpperLE(s); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.UpperGT(s); lt {
		tt.Fatal("should not be >=")
	}
	if lt := s.UpperGE(s); !lt {
		tt.Fatal("should be >=")
	}

	// compare t with itself
	if lt := t.LowerLT(t); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.LowerLE(t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.LowerGT(t); lt {
		tt.Fatal("should not be >")
	}
	if lt := t.LowerGE(t); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.UpperLT(t); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.UpperLE(t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.UpperGT(t); lt {
		tt.Fatal("should not be >=")
	}
	if lt := t.UpperGE(t); !lt {
		tt.Fatal("should be >=")
	}

	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.LowerLT(t); !lt {
		tt.Fatal("should be <")
	}
	if lt := s.LowerLE(t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.LowerGT(t); lt {
		tt.Fatal("should not be >")
	}
	if lt := s.LowerGE(t); lt {
		tt.Fatal("should not be >=")
	}
	if lt := t.LowerLT(s); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.LowerLE(s); lt {
		tt.Fatal("should not be <=")
	}
	if lt := t.LowerGT(s); !lt {
		tt.Fatal("should be >")
	}
	if lt := t.LowerGE(s); !lt {
		tt.Fatal("should be >=")
	}

	s, _ = NewSegment(&times[2],nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.UpperLT(t); lt {
		tt.Fatal("should not be <")
	}
	if lt := s.UpperLE(t); lt {
		tt.Fatal("should not be <=")
	}
	if lt := s.UpperGT(t); !lt {
		tt.Fatal("should be >")
	}
	if lt := s.UpperGE(t); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.UpperLT(s); !lt {
		tt.Fatal("should be <")
	}
	if lt := t.UpperLE(s); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.UpperGT(s); lt {
		tt.Fatal("should not be >")
	}
	if lt := t.UpperGE(s); lt {
		tt.Fatal("should not be >=")
	}

	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if lt := s.LowerLT(t); !lt {
		tt.Fatal("should be <")
	}
	if lt := s.LowerLE(t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.LowerGT(t); lt {
		tt.Fatal("should not be >")
	}
	if lt := s.LowerGE(t); lt {
		tt.Fatal("should not be >=")
	}
	if lt := t.LowerLT(s); !lt {
		tt.Fatal("should be <")
	}
	if lt := t.LowerLE(s); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.LowerGT(s); lt {
		tt.Fatal("should not be >")
	}
	if lt := t.LowerGE(s); lt {
		tt.Fatal("should not be >=")
	}

	s, _ = NewSegment(&times[2],nil, "[)")
	t, _ = NewSegment(&times[1], nil, "[)")
	if lt := s.UpperLT(t); lt {
		tt.Fatal("should not be <")
	}
	if lt := s.UpperLE(t); lt {
		tt.Fatal("should not be <=")
	}
	if lt := s.UpperGT(t); !lt {
		tt.Fatal("should be >")
	}
	if lt := s.UpperGE(t); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.UpperLT(s); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.UpperLE(s); lt {
		tt.Fatal("should not be <=")
	}
	if lt := t.UpperGT(s); !lt {
		tt.Fatal("should be >")
	}
	if lt := t.UpperGE(s); !lt {
		tt.Fatal("should be >=")
	}
}


func TestSegment_Contains(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if c := s.Contains(t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(s); c {
		tt.Fatal("should not contain")
	}

	s, _ = NewSegment(&times[0], &times[3], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if c := s.Contains(t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(s); c {
		tt.Fatal("should not contain")
	}
	s, t = t, s
	if c := s.Contains(t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(s); !c {
		tt.Fatal("should contain")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(s); !c {
		tt.Fatal("should contain")
	}
	s, t = t, s
	if c := s.Contains(t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(s); !c {
		tt.Fatal("should contain")
	}

	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(s); c {
		tt.Fatal("should not contain")
	}
	s, t = t, s
	if c := s.Contains(t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(s); !c {
		tt.Fatal("should contain")
	}

	s, _ = NewSegment(nil, nil, "()")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(s); c {
		tt.Fatal("should not contain")
	}
	s, t = t, s
	if c := s.Contains(t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(s); !c {
		tt.Fatal("should contain")
	}
	s = t
	t, _ = NewSegment(nil, &times[2], "()")
	if c := s.Contains(t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(s); c {
		tt.Fatal("should not contain")
	}
	s, t = t, s
	if c := s.Contains(t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(s); !c {
		tt.Fatal("should contain")
	}

	s = t
	t, _ = NewSegment(nil, nil, "()")
	if c := s.Contains(t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(s); !c {
		tt.Fatal("should contain")
	}
	s, t = t, s
	if c := s.Contains(t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(s); !c {
		tt.Fatal("should contain")
	}
}



//func TestInet(t *testing.T) {
//	conn := openTestConn(t)
//	defer conn.Close()
//
//	segment := Segment{}
//
//	//// Test scanning NULL values
//	rows, err := conn.QueryRow("SELECT NULL::segment")
//	if err != nil {
//		t.Fatalf("db error %s", err)
//	}
//	rows.Scan(&segment)
//	if segment.Valid {
//		t.Fatalf("expected null result")
//	}
//
//	// Test setting NULL values
//	rows, err = conn.QueryRow("SELECT $1::segment", segment)
//	if err != nil {
//		t.Fatalf("re-query null value failed: %s", err.Error())
//	}
//	rows.Scan(&segment)
//	if segment.Valid {
//		t.Fatalf("expected null result")
//	}
//
//	//test encoding in query params, then decoding during Scan
//	testBidirectional := func(s Segment, label string) {
//		rows, err = conn.QueryRow("SELECT $1::segment", s)
//		if err != nil {
//			t.Fatalf("re-query %s segment failed: %s", label, err.Error())
//		}
//		rows.Scan(&segment)
//		if !segment.Valid {
//			t.Fatalf("expected non-null value, got null for %s", label)
//		}
//
//		if segment.Segment.Equal(*s.Segment) != true {
//			t.Fatalf("expected segments to match, but did not for %s - \n%s \n%s\n\n%#v \n%#v", label, s.String(), segment.String(), s.Segment, segment.Segment)
//		}
//	}
//
//	// postgres truncates "00:00:00.000" to "00:00:00" so need extra milli
//	zeroT, _ := time.Parse(MIL_SEG_FORMAT, "00:00:00.000")
//	d, _ := time.ParseDuration("2h45m0s0us")
//	ts := timespan.New(zeroT, d)
//	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple time milli")
//
//	zeroT, _ = time.Parse(SEG_FORMAT, "00:00:00")
//	d, _ = time.ParseDuration("2h45m0s0us")
//	ts = timespan.New(zeroT, d)
//	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple time no milli")
//
//	zeroT, _ = time.Parse(SEG_FORMAT, "00:00:05")
//	d, _ = time.ParseDuration("2h45m")
//	ts = timespan.New(zeroT, d)
//	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple time no milli")
//
//	// Test setting mixed values
//	zeroT, _ = time.Parse(SEG_FORMAT, "00:18:34")
//	endT, _ := time.Parse(SEG_FORMAT, "00:22:46.99")
//	d = endT.Sub(zeroT)
//	ts = timespan.New(zeroT, d)
//	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple mixed")
//
//	// Test setting mixed values
//	zeroT, _ = time.Parse(SEG_FORMAT, "00:18:34.1")
//	endT, _ = time.Parse(SEG_FORMAT, "00:22:46")
//	d = endT.Sub(zeroT)
//	ts = timespan.New(zeroT, d)
//	testBidirectional(Segment{Segment: &ts, Valid: true}, "Simple mixed")
//
//	// Test empty
//	d = endT.Sub(endT)
//	ts = timespan.New(zeroT, d)
//	rows, err = conn.QueryRow("SELECT $1::segment", Segment{Segment: &ts, Valid: true})
//	err = rows.Scan(&segment)
//	if segment.Valid || err == nil {
//		t.Fatalf("expected err and invalid segment")
//	}
//
//	rows, err = conn.QueryRow("SELECT '(,)'::segment")
//	err = rows.Scan(&segment)
//	if !(segment.Valid && segment.Segment == nil) {
//		t.Fatalf("expected (,)")
//	}
//
//}
