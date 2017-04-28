// this is until https://github.com/lib/pq/pull/390 gets merged in

package custom_types

//import (
//	"bytes"
//	"net"
//	"testing"
//
//	"github.com/databrary/databrary/config"
//
//	"github.com/databrary/databrary/logging"
//)
//
//func TestInet(t *testing.T) {
//	config.InitConf("../../../config/databrary_test.toml")
//	conf := config.GetConf()
//	logging.InitLgr(conf)
//	conn := OpenTestConn(conf, t)
//	defer conn.Close()
//
//	inet := Inet{}
//
//	// Test scanning NULL values
//	rows, err := conn.QueryRow("SELECT NULL::inet")
//	if err != nil {
//		t.Fatalf("db error %s", err)
//	}
//	rows.Scan(&inet)
//	if inet.Valid {
//		t.Fatalf("expected null result")
//	}
//
//	// Test setting NULL values
//	rows, err = conn.QueryRow("SELECT $1::inet", inet)
//	if err != nil {
//		t.Fatalf("re-query null value failed: %s", err.Error())
//	}
//	rows.Scan(&inet)
//	if inet.Valid {
//		t.Fatalf("expected null result")
//	}
//
//	// test encoding in query params, then decoding during Scan
//	testBidirectional := func(i Inet, label string) {
//		rows, err = conn.QueryRow("SELECT $1::inet", i)
//		if err != nil {
//			t.Fatalf("re-query %s inet failed: %s", label, err.Error())
//		}
//		rows.Scan(&inet)
//		if !inet.Valid {
//			t.Fatalf("expected non-null value, got null for %s", label)
//		}
//		if bytes.Compare(i.Inet, inet.Inet) != 0 {
//			t.Fatalf("expected IP addresses to match, but did not for %s - %s %s", label, i.Inet.String(), inet.Inet.String())
//		}
//	}
//
//	testBidirectional(Inet{Inet: net.ParseIP("192.168.0.1"), Valid: true}, "Simple IPv4")
//	testBidirectional(Inet{Inet: net.ParseIP("::1"), Valid: true}, "Loopback IPv6")
//	testBidirectional(Inet{Inet: net.ParseIP("abcd:2345::"), Valid: true}, "Loopback IPv6")
//
//	// Bad argument
//	inet = Inet{}
//	err = inet.Scan(456)
//	if err == nil {
//		t.Fatal("Expected error for non-byte[] argument to Scan")
//	}
//
//	inet = Inet{}
//	err = inet.Scan([]byte(""))
//	if err != nil {
//		t.Fatalf("Unexpected error for empty string - %s", err.Error())
//	}
//	if inet.Valid {
//		t.Fatalf("Unexpected not null for empty/non-IP string string")
//	}
//}
