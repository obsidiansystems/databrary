package custom_types

import (
	"testing"
	"time"

	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	"github.com/jmoiron/sqlx"
	"os"
	"path/filepath"
)

var dbConn *sqlx.DB

func TestSegment(t *testing.T) {
	var err error
	GOPATH := os.Getenv("GOPATH")
	config.InitConf(filepath.Join(GOPATH, "src/github.com/databrary/databrary/config/databrary_test.toml"))
	conf := config.GetConf()
	// initialize db connection
	err = db.InitDB(conf)
	if err != nil {
		panic(err.Error())
	}
	defer dbConn.Close()

	t.Run("testNewSegment", testNewSegment)
	t.Run("testSegment_Contains", testSegment_Contains)
	t.Run("testSegment_Duration", testSegment_Duration)
	t.Run("testSegment_Equal", testSegment_Equal)
	t.Run("testSegment_IsSingleton", testSegment_IsSingleton)
	t.Run("testSegment_LowerGE", testSegment_LowerGE)
	t.Run("testSegment_LowerGT", testSegment_LowerGT)
	t.Run("testSegment_LowerLE", testSegment_LowerLE)
	t.Run("testSegment_LowerLT", testSegment_LowerLT)
	t.Run("testSegment_Minus", testSegment_Minus)
	t.Run("testSegment_UpperGE", testSegment_UpperGE)
	t.Run("testSegment_UpperGT", testSegment_UpperGT)
	t.Run("testSegment_UpperLE", testSegment_UpperLE)
	t.Run("testSegment_UpperLT", testSegment_UpperLT)
	t.Run("testNullSegment", testNullSegment)
	t.Run("testSegment_Scan", testSegment_Scan)
	//should be last since it mutates intervals
	t.Run("testSegment_Shift", testSegment_Shift)

}

var d1, _ = time.ParseDuration("1m")
var d2, _ = time.ParseDuration("2m")
var d3, _ = time.ParseDuration("3m")
var d4, _ = time.ParseDuration("4m")
var times = []Interval{NewInterval(d1), NewInterval(d2), NewInterval(d3), NewInterval(d4)}

func testNewSegment(t *testing.T) {
	// test empty
	if _, e := NewSegment(nil, nil, ""); e != nil {
		t.Error(e.Error())
	}
	// test empty erroring
	if _, e := NewSegment(&times[0], nil, ""); e == nil {
		t.Error("empty bounds with non-nil lower")
	}
	if _, e := NewSegment(nil, &times[0], ""); e == nil {
		t.Error("empty bounds with non-nil upper")
	}
	if _, e := NewSegment(&times[0], &times[1], ""); e == nil {
		t.Error("empty bounds with non-nil lower and upper")
	}

	// test finite
	if _, e := NewSegment(&times[0], &times[1], "[]"); e != nil {
		t.Error(e)
	}
	if _, e := NewSegment(&times[0], &times[1], "[)"); e != nil {
		t.Error(e)
	}
	if _, e := NewSegment(&times[0], &times[1], "(]"); e != nil {
		t.Error(e)
	}
	if _, e := NewSegment(&times[0], &times[1], "()"); e != nil {
		t.Error(e)
	}
	// test finite errors - flipped bounds
	if _, e := NewSegment(&times[1], &times[0], "[]"); e == nil {
		t.Error("flipped bounds should produce error")
	}

	// test lower unbound
	if _, e := NewSegment(nil, &times[0], "(]"); e != nil {
		t.Error(e)
	}
	if _, e := NewSegment(nil, &times[0], "()"); e != nil {
		t.Error(e)
	}
	if _, e := NewSegment(nil, nil, "()"); e != nil {
		t.Error(e)
	}
	// test lower unbound error
	if _, e := NewSegment(nil, &times[0], "[]"); e == nil {
		t.Error("[ for nil lower bound should error")
	}
	if _, e := NewSegment(nil, nil, "[]"); e == nil {
		t.Error("[] for both nil bounds should error")
	}

	// test upper unbound
	if _, e := NewSegment(&times[0], nil, "[)"); e != nil {
		t.Error(e)
	}
	// test upper unbound error
	if _, e := NewSegment(&times[0], nil, "[]"); e == nil {
		t.Error("upper nil with ] should error")
	}
}

func testSegment_LowerLT(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if lt := s.LowerLT(&t); !lt {
		tt.Error("should be <")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")
	if lt := s.LowerLT(&t); lt {
		tt.Error("should not be <")
	}
	if lt := t.LowerLT(&s); lt {
		tt.Error("should not be <")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.LowerLT(&s); lt {
		tt.Error("should not be <")
	}
	if lt := t.LowerLT(&t); lt {
		tt.Error("should not be <")
	}

	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.LowerLT(&t); !lt {
		tt.Error("should be <")
	}
	if lt := t.LowerLT(&s); lt {
		tt.Error("should not be <")
	}

	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if lt := s.LowerLT(&t); !lt {
		tt.Error("should be <")
	}
	if lt := t.LowerLT(&s); !lt {
		tt.Error("should be <")
	}
}

func testSegment_LowerLE(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if lt := s.LowerLE(&t); !lt {
		tt.Error("should be <=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")
	if lt := s.LowerLE(&t); !lt {
		tt.Error("should be <=")
	}
	if lt := t.LowerLE(&s); !lt {
		tt.Error("should be <=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.LowerLE(&s); !lt {
		tt.Error("should be <=")
	}
	if lt := t.LowerLE(&t); !lt {
		tt.Error("should be <=")
	}

	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.LowerLE(&t); !lt {
		tt.Error("should be <=")
	}
	if lt := t.LowerLE(&s); lt {
		tt.Error("should not be <=")
	}

	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if lt := s.LowerLE(&t); !lt {
		tt.Error("should be <=")
	}
	if lt := t.LowerLE(&s); !lt {
		tt.Error("should be <=")
	}

}

func testSegment_LowerGT(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if gt := s.LowerGT(&t); gt {
		tt.Error("should not be >")
	}
	if gt := t.LowerGT(&s); !gt {
		tt.Error("should be >")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")
	if gt := s.LowerGT(&t); gt {
		tt.Error("should not be >")
	}
	if gt := t.LowerGT(&s); gt {
		tt.Error("should not be >")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if gt := s.LowerGT(&s); gt {
		tt.Error("should not be >")
	}
	if gt := t.LowerGT(&t); gt {
		tt.Error("should not be >")
	}

	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if gt := s.LowerGT(&t); gt {
		tt.Error("should not be >")
	}
	if gt := t.LowerGT(&s); !gt {
		tt.Error("should be >")
	}

	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if gt := s.LowerGT(&t); gt {
		tt.Error("should not be >")
	}
	if gt := t.LowerGT(&s); gt {
		tt.Error("should not be >")
	}
}

func testSegment_LowerGE(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if ge := s.LowerGE(&t); ge {
		tt.Error("should not be >=")
	}
	if ge := t.LowerGE(&s); !ge {
		tt.Error("should be >=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")
	if ge := s.LowerGE(&t); !ge {
		tt.Error("should be >=")
	}
	if ge := t.LowerGE(&s); !ge {
		tt.Error("should be >=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if ge := s.LowerGE(&s); !ge {
		tt.Error("should be >=")
	}
	if ge := t.LowerGE(&t); !ge {
		tt.Error("should be >=")
	}

	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if ge := s.LowerGE(&t); ge {
		tt.Error("should not be >=")
	}
	if ge := t.LowerGE(&s); !ge {
		tt.Error("should be >=")
	}

	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if ge := s.LowerGE(&t); ge {
		tt.Error("should not be >=")
	}
	if ge := t.LowerGE(&s); ge {
		tt.Error("should not be >=")
	}
}

func testSegment_UpperLT(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if lt := s.UpperLT(&t); !lt {
		tt.Error("should be <")
	}
	if lt := t.UpperLT(&s); lt {
		tt.Error("should not be <")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")
	if lt := s.UpperLT(&t); !lt {
		tt.Error("should be <")
	}
	if lt := t.UpperLT(&s); lt {
		tt.Error("should not be <")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.UpperLT(&t); lt {
		tt.Error("should not be <")
	}
	if lt := s.UpperLT(&s); lt {
		tt.Error("should not be <")
	}
	if lt := t.UpperLT(&s); lt {
		tt.Error("should not be <")
	}
	if lt := t.UpperLT(&t); lt {
		tt.Error("should not be <")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.UpperLT(&t); lt {
		tt.Error("should not be <")
	}
	if lt := t.UpperLT(&s); !lt {
		tt.Error("should be <")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], nil, "[)")
	if lt := s.UpperLT(&t); lt {
		tt.Error("should not be <")
	}
	if lt := t.UpperLT(&s); lt {
		tt.Error("should not be <")
	}

}

func testSegment_UpperLE(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if le := s.UpperLE(&t); !le {
		tt.Error("should be <=")
	}
	if le := t.UpperLE(&s); le {
		tt.Error("should not be <=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if le := s.UpperLE(&t); !le {
		tt.Error("should be <=")
	}
	if le := t.UpperLE(&s); !le {
		tt.Error("should be <=")
	}
	if le := s.UpperLE(&s); !le {
		tt.Error("should be <=")
	}
	if le := t.UpperLE(&t); !le {
		tt.Error("should be <=")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if le := s.UpperLE(&t); le {
		tt.Error("should not be <=")
	}
	if le := t.UpperLE(&s); !le {
		tt.Error("should be <=")
	}

	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if le := t.UpperLE(&s); !le {
		tt.Error("should be <=")
	}

}

func testSegment_UpperGT(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if gt := s.UpperGT(&t); gt {
		tt.Error("should not be >")
	}
	if gt := t.UpperGT(&s); !gt {
		tt.Error("should be >")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if gt := s.UpperGT(&t); gt {
		tt.Error("should not be >=")
	}
	if gt := t.UpperGT(&s); gt {
		tt.Error("should not be >")
	}
	if gt := s.UpperGT(&s); gt {
		tt.Error("should not be >=")
	}
	if gt := t.UpperGT(&t); gt {
		tt.Error("should not be >=")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if gt := s.UpperGT(&t); !gt {
		tt.Error("should be >")
	}
	if gt := t.UpperGT(&s); gt {
		tt.Error("should not be >")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], nil, "[)")
	if gt := s.UpperGT(&t); !gt {
		tt.Error("should be >")
	}
	if gt := t.UpperGT(&s); !gt {
		tt.Error("should be >")
	}

}

func testSegment_UpperGE(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if ge := s.UpperGE(&t); ge {
		tt.Error("should not be >=")
	}
	if ge := t.UpperGE(&s); !ge {
		tt.Error("should be >=")
	}
	// compare s with itself
	if ge := s.UpperGE(&s); !ge {
		tt.Error("should be >=")
	}
	// compare t with itself
	if ge := t.UpperGE(&t); !ge {
		tt.Error("should be >=")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if ge := s.UpperGE(&t); !ge {
		tt.Error("should be >=")
	}
	if ge := t.UpperGE(&s); ge {
		tt.Error("should not be >=")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], nil, "[)")
	if ge := s.UpperGE(&t); !ge {
		tt.Error("should be >=")
	}
	if ge := t.UpperGE(&s); !ge {
		tt.Error("should be >=")
	}

}

func testSegment_Equal(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	if eq := s.Equal(&s); !eq {
		tt.Error("should be equal")
	}

	s, _ = NewSegment(nil, &times[1], "()")
	if eq := s.Equal(&s); !eq {
		tt.Error("should be equal")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	if eq := s.Equal(&s); !eq {
		tt.Error("should be equal")
	}

	s, _ = NewSegment(nil, nil, "()")
	if eq := s.Equal(&s); !eq {
		tt.Error("should be equal")
	}
}

func testSegment_Contains(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if c := s.Contains(&t); c {
		tt.Error("should not contain")
	}
	if c := t.Contains(&s); c {
		tt.Error("should not contain")
	}

	s, _ = NewSegment(&times[0], &times[3], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if c := s.Contains(&t); !c {
		tt.Error("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Error("should not contain")
	}
	s, t = t, s
	if c := s.Contains(&t); c {
		tt.Error("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Error("should contain")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(&t); !c {
		tt.Error("should contain")
	}
	if c := t.Contains(&s); !c {
		tt.Error("should contain")
	}
	s, t = t, s
	if c := s.Contains(&t); !c {
		tt.Error("should contain")
	}
	if c := t.Contains(&s); !c {
		tt.Error("should contain")
	}

	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(&t); !c {
		tt.Error("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Error("should not contain")
	}
	s, t = t, s
	if c := s.Contains(&t); c {
		tt.Error("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Error("should contain")
	}

	s, _ = NewSegment(nil, nil, "()")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(&t); !c {
		tt.Error("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Error("should not contain")
	}
	s, t = t, s
	if c := s.Contains(&t); c {
		tt.Error("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Error("should contain")
	}

	s, _ = NewSegment(&times[0], &times[2], "()")
	t, _ = NewSegment(nil, &times[2], "()")
	if c := s.Contains(&t); c {
		tt.Error("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Error("should contain")
	}
	s, t = t, s
	if c := s.Contains(&t); !c {
		tt.Error("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Error("should not contain")
	}

	s, _ = NewSegment(nil, &times[2], "()")
	t, _ = NewSegment(nil, nil, "()")
	if c := s.Contains(&t); c {
		tt.Error("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Error("should contain")
	}
	s, t = t, s
	if c := s.Contains(&t); !c {
		tt.Error("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Error("should not contain")
	}
}

func testSegment_IsSingleton(t *testing.T) {
	s, _ := NewSegment(&times[0], &times[0], "[]")
	if i := s.IsSingleton(); !i {
		t.Fatalf("should be singleton %s", s)
	}
	s, _ = NewSegment(&times[0], &times[1], "[]")
	if i := s.IsSingleton(); i {
		t.Fatalf("should not be singleton %s", s)
	}
}

func testSegment_Duration(t *testing.T) {
	s, _ := NewSegment(&times[0], &times[1], "[]")
	if d := s.Duration(); d != 60000000000 {
		t.Fatalf("wrong duration %d", int(d))
	}
}

func testSegment_Shift(t *testing.T) {
	d1, _ := time.ParseDuration("1m")
	d2, _ := time.ParseDuration("2m")
	d3, _ := time.ParseDuration("3m")
	d4, _ := time.ParseDuration("4m")
	s := Segment{lower: &Interval{d1}, upper: &Interval{d2}, bounds: "[]"}
	d, _ := time.ParseDuration("2m")
	s.Shift(d)
	ss := Segment{lower: &Interval{d3}, upper: &Interval{d4}, bounds: "[]"}
	if e := ss.Equal(&s); !e {
		t.Fatalf("should be equal %s %s", s, s)
	}
}

func testSegment_Minus(t *testing.T) {
	s, _ := NewSegment(&times[0], &times[1], "[]")
	ss, _ := NewSegment(&times[1], &times[2], "[]")
	if m := s.Minus(&ss); m != 0 {
		t.Fatalf("difference %s should be 0", m)
	}
}

func testSegment_Scan(t *testing.T) {
	var err error
	GOPATH := os.Getenv("GOPATH")
	config.InitConf(filepath.Join(GOPATH, "src/github.com/databrary/databrary/config/databrary_test.toml"))
	conf := config.GetConf()
	err = db.InitDB(conf)
	if err != nil {
		panic(err.Error())
	}

	defer dbConn.Close()

	var segment *Segment

	testBidirectional := func(s Segment, label string) {
		rows, err := dbConn.Query("SELECT $1::segment", s)
		if err != nil {
			t.Fatalf("re-query %s segment failed: %s", label, err)
		}
		if rows.Next() {
			err = rows.Scan(&segment)
			if segment == nil || err != nil {
				t.Fatalf("expected non-null value, got null for %s or error %+v", label, err)
			}
			if segment.Equal(&s) != true {
				t.Fatalf("expected segments to match, but did not for %s - \n%s \n%s", label, s, segment)
			}

		} else {
			t.Fatalf("no results for %s", label)
		}
	}
	//
	start := Interval{}
	end := NewInterval(start.interval + 7200000000000)
	end2 := NewInterval(start.interval + 9200000000000)
	seg, _ := NewSegment(&start, &end, "[]")
	testBidirectional(seg, "1")
	seg, _ = NewSegment(&start, &end, "(]")
	testBidirectional(seg, "2")
	seg, _ = NewSegment(&start, &end, "[)")
	testBidirectional(seg, "3")
	seg, _ = NewSegment(&start, &end, "()")
	testBidirectional(seg, "4")
	seg, _ = NewSegment(&end, &end2, "[]")
	testBidirectional(seg, "5")
	seg, _ = NewSegment(&end, &end2, "(]")
	testBidirectional(seg, "6")
	seg, _ = NewSegment(&end, &end2, "[)")
	testBidirectional(seg, "7")
	seg, _ = NewSegment(&end, &end2, "()")
	testBidirectional(seg, "8")
	seg, _ = NewSegment(nil, &end2, "(]")
	testBidirectional(seg, "9")
	seg, _ = NewSegment(&end, nil, "[)")
	testBidirectional(seg, "10")
	seg, _ = NewSegment(nil, nil, "()")
	testBidirectional(seg, "11")
	seg, _ = NewSegment(nil, nil, "")
	testBidirectional(seg, "12")

}

func testNullSegment(t *testing.T) {

	var segment *NullSegment

	//// Test scanning NULL values
	rows, err := dbConn.Query("SELECT NULL::segment")
	if err != nil {
		t.Fatalf("db error %s", err)
	}
	if rows.Next() {
		err := rows.Scan(&segment)
		if err != nil {
			t.Fatal(err)
		}
		if segment != nil {
			t.Fatalf("expected null result")
		}
	} else {
		t.Fatal("no results for ")
	}

	// Test setting NULL values
	rows, err = dbConn.Query("SELECT $1::segment", segment)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	if rows.Next() {
		err := rows.Scan(&segment)
		if err != nil {
			t.Fatal(err)
		}
		if segment != nil {
			t.Fatalf("expected null result")
		}
	} else {
		t.Fatal("no results for test null segment")
	}

}
