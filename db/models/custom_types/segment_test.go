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

var connSegment *sqlx.DB

func TestSegment(t *testing.T) {
	var err error
	GOPATH := os.Getenv("GOPATH")
	config.InitConf(filepath.Join(GOPATH, "src/github.com/databrary/databrary/config/databrary_test.toml"))
	conf := config.GetConf()
	connSegment, err = db.OpenConn(conf)
	if err != nil {
		t.Fatal(err)
	}
	defer connSegment.Close()

	t.Run("", testNewSegment)
	t.Run("", testSegment_Contains)
	t.Run("", testSegment_Duration)
	t.Run("", testSegment_Equal)
	t.Run("", testSegment_IsSingleton)
	t.Run("", testSegment_LowerGE)
	t.Run("", testSegment_LowerGT)
	t.Run("", testSegment_LowerLE)
	t.Run("", testSegment_LowerLT)
	t.Run("", testSegment_Minus)
	t.Run("", testSegment_UpperGE)
	t.Run("", testSegment_UpperGT)
	t.Run("", testSegment_UpperLE)
	t.Run("", testSegment_UpperLT)
	t.Run("", testNullSegment)
	t.Run("", testSegment_Scan)
	//should be last since it mutates intervals
	t.Run("", testSegment_Shift)

}

var d1, _ = time.ParseDuration("1m")
var d2, _ = time.ParseDuration("2m")
var d3, _ = time.ParseDuration("3m")
var d4, _ = time.ParseDuration("4m")
var times = []Interval{NewInterval(d1), NewInterval(d2), NewInterval(d3), NewInterval(d4)}

func testNewSegment(t *testing.T) {
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

func testSegment_LowerLT(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if lt := s.LowerLT(&t); !lt {
		tt.Fatal("should be <")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")
	if lt := s.LowerLT(&t); lt {
		tt.Fatal("should not be <")
	}

	if lt := t.LowerLT(&s); lt {
		tt.Fatal("should not be <")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.LowerLT(&s); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.LowerLT(&t); lt {
		tt.Fatal("should not be <")
	}
	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.LowerLT(&t); !lt {
		tt.Fatal("should be <")
	}
	if lt := t.LowerLT(&s); lt {
		tt.Fatal("should not be <")
	}
	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if lt := s.LowerLT(&t); !lt {
		tt.Fatal("should be <")
	}
	if lt := t.LowerLT(&s); !lt {
		tt.Fatal("should be <")
	}
}

func testSegment_LowerLE(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")

	if lt := s.LowerLE(&t); !lt {
		tt.Fatal("should be <=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")

	if lt := s.LowerLE(&t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.LowerLE(&s); !lt {
		tt.Fatal("should be <=")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.LowerLE(&s); !lt {
		tt.Fatal("should be <=")
	}
	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.LowerLE(&t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.LowerLE(&s); lt {
		tt.Fatal("should not be <=")
	}
	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if lt := s.LowerLE(&t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.LowerLE(&s); !lt {
		tt.Fatal("should be <=")
	}

}

func testSegment_LowerGT(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")

	if lt := s.LowerGT(&t); lt {
		tt.Fatal("should not be >")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")

	if lt := s.LowerGT(&t); lt {
		tt.Fatal("should not be >")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.LowerGT(&s); lt {
		tt.Fatal("should not be >")
	}
	if lt := t.LowerGT(&t); lt {
		tt.Fatal("should not be >")
	}
	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.LowerGT(&t); lt {
		tt.Fatal("should not be >")
	}
	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if lt := s.LowerGT(&t); lt {
		tt.Fatal("should not be >")
	}
}

func testSegment_LowerGE(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")

	if lt := s.LowerGE(&t); lt {
		tt.Fatal("should not be >=")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")

	if lt := s.LowerGE(&t); !lt {
		tt.Fatal("should be >=")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.LowerGE(&s); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.LowerGE(&t); !lt {
		tt.Fatal("should be >=")
	}
	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.LowerGE(&t); lt {
		tt.Fatal("should not be >=")
	}
	if lt := t.LowerGE(&s); !lt {
		tt.Fatal("should be >=")
	}
	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if lt := s.LowerGE(&t); lt {
		tt.Fatal("should not be >=")
	}
	if lt := t.LowerGT(&s); lt {
		tt.Fatal("should not be >")
	}
}

func testSegment_UpperLT(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")

	if lt := s.UpperLT(&t); !lt {
		tt.Fatal("should be <")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")

	if lt := t.UpperLT(&s); lt {
		tt.Fatal("should not be <")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.UpperLT(&t); lt {
		tt.Fatal("should not be <")
	}
	if lt := s.UpperLT(&s); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.UpperLT(&s); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.UpperLT(&t); lt {
		tt.Fatal("should not be <")
	}
	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.UpperLT(&t); lt {
		tt.Fatal("should not be <")
	}
	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := t.UpperLT(&s); !lt {
		tt.Fatal("should be <")
	}
	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], nil, "[)")
	if lt := s.UpperLT(&t); lt {
		tt.Fatal("should not be <")
	}
	if lt := t.UpperLT(&s); lt {
		tt.Fatal("should not be <")
	}

}

func testSegment_UpperLE(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")

	if lt := s.UpperLE(&t); !lt {
		tt.Fatal("should be <=")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[3], "(]")

	if lt := t.UpperLE(&s); lt {
		tt.Fatal("should not be <=")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.UpperLE(&t); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.UpperLE(&s); !lt {
		tt.Fatal("should be <=")
	}
	if lt := s.UpperLE(&s); !lt {
		tt.Fatal("should be <=")
	}
	if lt := t.UpperLE(&t); !lt {
		tt.Fatal("should be <=")
	}
	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.UpperLE(&t); lt {
		tt.Fatal("should not be <=")
	}
	if lt := t.UpperLE(&s); !lt {
		tt.Fatal("should be <=")
	}
	s, _ = NewSegment(nil, &times[1], "()")
	t, _ = NewSegment(nil, &times[0], "()")
	if lt := t.UpperLE(&s); !lt {
		tt.Fatal("should be <=")
	}

}

func testSegment_UpperGT(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")

	if lt := s.UpperGT(&t); lt {
		tt.Fatal("should not be >")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.UpperGT(&t); lt {
		tt.Fatal("should not be >=")
	}
	if lt := t.UpperGT(&s); lt {
		tt.Fatal("should not be >")
	}
	if lt := s.UpperGT(&s); lt {
		tt.Fatal("should not be >=")
	}
	if lt := t.UpperGT(&t); lt {
		tt.Fatal("should not be >=")
	}
	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.UpperGT(&t); !lt {
		tt.Fatal("should be >")
	}
	if lt := t.UpperGT(&s); lt {
		tt.Fatal("should not be >")
	}
	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], nil, "[)")
	if lt := s.UpperGT(&t); !lt {
		tt.Fatal("should be >")
	}
	if lt := t.UpperGT(&s); !lt {
		tt.Fatal("should be >")
	}

}

func testSegment_UpperGE(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")

	if lt := s.UpperGE(&t); lt {
		tt.Fatal("should not be >=")
	}
	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[)")
	if lt := s.UpperGE(&t); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.UpperGE(&s); !lt {
		tt.Fatal("should be >=")
	}

	// compare s with itself
	if lt := s.UpperGE(&s); !lt {
		tt.Fatal("should be >=")
	}

	// compare t with itself
	if lt := t.UpperGE(&t); !lt {
		tt.Fatal("should be >=")
	}
	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if lt := s.UpperGE(&t); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.UpperGE(&s); lt {
		tt.Fatal("should not be >=")
	}
	s, _ = NewSegment(&times[2], nil, "[)")
	t, _ = NewSegment(&times[1], nil, "[)")
	if lt := s.UpperGE(&t); !lt {
		tt.Fatal("should be >=")
	}
	if lt := t.UpperGE(&s); !lt {
		tt.Fatal("should be >=")
	}

}

func testSegment_Equal(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")

	if eq := s.Equal(&s); !eq {
		tt.Fatal("should be equal")
	}
	s, err := NewSegment(nil, &times[1], "()")
	if err != nil {
		tt.Fatal(err)
	}
	if eq := s.Equal(&s); !eq {
		tt.Fatal("should be equal")
	}

	s, _ = NewSegment(&times[2], nil, "[)")
	if eq := s.Equal(&s); !eq {
		tt.Fatal("should be equal")
	}

	s, _ = NewSegment(nil, nil, "()")
	if eq := s.Equal(&s); !eq {
		tt.Fatal("should be equal")
	}
}

func testSegment_Contains(tt *testing.T) {
	s, _ := NewSegment(&times[0], &times[2], "[]")
	t, _ := NewSegment(&times[1], &times[3], "[]")
	if c := s.Contains(&t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(&s); c {
		tt.Fatal("should not contain")
	}

	s, _ = NewSegment(&times[0], &times[3], "[]")
	t, _ = NewSegment(&times[1], &times[2], "[]")
	if c := s.Contains(&t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Fatal("should not contain")
	}
	s, t = t, s
	if c := s.Contains(&t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Fatal("should contain")
	}

	s, _ = NewSegment(&times[0], &times[2], "[]")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(&t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(&s); !c {
		tt.Fatal("should contain")
	}
	s, t = t, s
	if c := s.Contains(&t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(&s); !c {
		tt.Fatal("should contain")
	}

	s, _ = NewSegment(nil, &times[2], "(]")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(&t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Fatal("should not contain")
	}
	s, t = t, s
	if c := s.Contains(&t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Fatal("should contain")
	}

	s, _ = NewSegment(nil, nil, "()")
	t, _ = NewSegment(&times[0], &times[2], "()")
	if c := s.Contains(&t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Fatal("should not contain")
	}
	s, t = t, s
	if c := s.Contains(&t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Fatal("should contain")
	}
	s = t
	t, _ = NewSegment(nil, &times[2], "()")
	if c := s.Contains(&t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(&s); c {
		tt.Fatal("should not contain")
	}
	s, t = t, s
	if c := s.Contains(&t); c {
		tt.Fatal("should not contain")
	}
	if c := t.Contains(&s); !c {
		tt.Fatal("should contain")
	}

	s = t
	t, _ = NewSegment(nil, nil, "()")
	if c := s.Contains(&t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(&s); !c {
		tt.Fatal("should contain")
	}
	s, t = t, s
	if c := s.Contains(&t); !c {
		tt.Fatal("should contain")
	}
	if c := t.Contains(&s); !c {
		tt.Fatal("should contain")
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
	connSegment, err = db.OpenConn(conf)
	if err != nil {
		t.Fatal(err)
	}
	defer connSegment.Close()

	var segment *Segment

	testBidirectional := func(s Segment, label string) {
		rows, err := connSegment.Query("SELECT $1::segment", s)
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
	rows, err := connSegment.Query("SELECT NULL::segment")
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
	rows, err = connSegment.Query("SELECT $1::segment", segment)
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
