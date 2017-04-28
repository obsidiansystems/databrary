package custom_types

import (
	"testing"

	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	"os"
	"path/filepath"
	"time"
	"fmt"
)

func TestInterval(t *testing.T) {
	GOPATH := os.Getenv("GOPATH")
	config.InitConf(filepath.Join(GOPATH,"src/github.com/databrary/databrary/config/databrary_test.toml"))
	conf := config.GetConf()
	conn, err := db.OpenConn(conf)
	if err != nil {
		t.Fatal("failed to open db conn")
	}
	defer conn.Close()

	inter := Interval{}

	//// Test scanning NULL values
	//rows, err := conn.QueryRow("SELECT NULL::interval HOUR TO SECOND (3)")
	//if err != nil {
	//	t.Fatalf("db error %s", err)
	//}
	//
	//err = rows.Scan(&inter)
	//if err != nil {
	//	t.Fatalf("db error %+v", err)
	//}
	//
	//// Test setting NULL values
	//rows, err = conn.QueryRow("SELECT $1::inet", inter)
	//if err != nil {
	//	t.Fatalf("re-query null value failed: %s", err.Error())
	//}
	//err = rows.Scan(&inter)
	//if err != nil {
	//	t.Fatalf("db error %s", err)
	//}

	// test encoding in query params, then decoding during Scan
	testBidirectional := func(label string, i Interval) {
		rows, err := conn.QueryRow("SELECT $1::interval HOUR TO SECOND (3)", i)
		if err != nil {
			t.Errorf("re-query %s failed: %s", label, err.Error())
		}
		err = rows.Scan(&inter)
		if err != nil {
			t.Fatal(err)
		}
		if i.interval != inter.interval {
			t.Fatalf("expected intervals %s to match, but did not for %#v %#v", label, inter, i)
		}
		fmt.Println(inter.interval)
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
