package custom_types

import (
	"testing"
	"time"

	"github.com/databrary/databrary/config"
	//. "github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/databrary/logging"
)


func TestSegment(t *testing.T) {
	config.InitConf("../../../config/databrary_test.toml")
	conf := config.GetConf()
	logging.InitLgr(conf)

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
	t.Run("", testSegment_Shift)
	t.Run("", testSegment_UpperGE)
	t.Run("", testSegment_UpperGT)
	t.Run("", testSegment_UpperLE)
	t.Run("", testSegment_UpperLT)

}

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

	if lt := t.UpperLE(&s); !lt {
		tt.Fatal("should be <=")
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
	s, _ = NewSegment(nil, &times[1], "()")
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
	if d := s.Duration(); d != 7200000000000 {
		t.Fatalf("wrong duration %d", int(d))
	}
}

func testSegment_Shift(t *testing.T) {
	s, _ := NewSegment(&times[0], &times[1], "[]")
	s.Shift(time.Duration(7200000000000))
	ss, _ := NewSegment(&times[1], &times[2], "[]")
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

func TestSegment_Scan(t *testing.T) {
	config.InitConf("../../../config/databrary_test.toml")
	conf := config.GetConf()
	logging.InitLgr(conf)
	conn := OpenTestConn(conf, t)
	defer conn.Close()

	var segment *Segment

	//// Test scanning NULL values
	rows, err := conn.QueryRow("SELECT NULL::segment")
	if err != nil {
		t.Fatalf("db error %s", err)
	}
	rows.Scan(&segment)
	if segment != nil {
		t.Fatalf("expected null result")
	}

	// Test setting NULL values
	rows, err = conn.QueryRow("SELECT $1::segment", segment)
	if err != nil {
		t.Fatalf("re-query null value failed: %s", err.Error())
	}
	rows.Scan(&segment)
	if segment != nil {
		t.Fatalf("expected null result")
	}
	//
	//test encoding in query params, then decoding during Scan
	testBidirectional := func(s Segment, label string) {
		rows, err = conn.QueryRow("SELECT $1::segment", s)
		if err != nil {
			t.Fatalf("re-query %s segment failed: %s", label, err.Error())
		}
		err := rows.Scan(&segment)
		if segment == nil || err != nil {
			t.Fatalf("expected non-null value, got null for %s or error %s", label, err)
		}

		if segment.Equal(&s) != true {
			t.Fatalf("expected segments to match, but did not for %s - \n%s \n%s\n\n%#v \n%#v", label, s.String(), segment.String(), s, segment)
		}
	}
	//
	start := time.Time{}
	end := start.Add(7200000000000)
	end2 := start.Add(9200000000000)
	seg, _ := NewSegment(&start, &end, "[]")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(&start, &end, "(]")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(&start, &end, "[)")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(&start, &end, "()")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(&end, &end2, "[]")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(&end, &end2, "(]")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(&end, &end2, "[)")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(&end, &end2, "()")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(nil, &end2, "(]")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(&end, nil, "[)")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(nil, nil, "()")
	testBidirectional(seg, "Simple time milli")
	seg, _ = NewSegment(nil, nil, "")
	testBidirectional(seg, "Simple time milli")

}
