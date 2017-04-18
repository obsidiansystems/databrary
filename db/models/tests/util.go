package tests

import (
	"fmt"
	"io/ioutil"
	"strings"
	"testing"

	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	"github.com/spf13/viper"
	"upper.io/db.v3/lib/sqlbuilder"
)

func OpenTestConn(conf *viper.Viper, t *testing.T) sqlbuilder.Database {

	if conn, err := db.OpenConn(conf); err != nil {
		t.Fatal(err)
		return nil
	} else {
		return conn
	}

}

/*
 this is an ugly global hack because go doesn't let you inject dependencies
 into test functions. so each test file needs to use the databrary
 database should create something like

 func TestVolume(t *testing.T) {
	testFuncs = []testFunc{
		testFunc{"", testVolume},
		testFunc{"", testVolumeAccess},
		testFunc{"", testVolumeOwners},
		testFunc{"", testVolumeLink},
		testFunc{"", testVolumeCitatation},
		testFunc{"", testFunder},
		testFunc{"", testVolumeFunding},
	}
	test(t)
 }


*/

type testFunc struct {
	name         string
	testFunction func(t *testing.T)
}

var testFuncs []testFunc
var testConn sqlbuilder.Database

func test(t *testing.T) {
	config.InitConf("../../../config/databrary_test.toml")
	conf := config.GetConf()
	logging.InitLgr(conf)

	// create test db
	testConn = OpenTestConn(conf, t)
	testSchemaDbName := strings.ToLower(fmt.Sprintf("testdb_%s", util.RandStringRunes(10)))
	_, err := testConn.Exec(fmt.Sprintf("CREATE DATABASE %s", testSchemaDbName))
	util.CheckErr(err)
	err = testConn.Close()
	util.CheckErr(err)

	// install schema
	conf.Set("database.db_name", testSchemaDbName)
	util.CheckErr(err)
	testConn = OpenTestConn(conf, t)
	schemaFile, err := ioutil.ReadFile(conf.GetString("database.schema"))
	util.CheckErr(err)
	_, err = testConn.Exec(string(schemaFile))
	util.CheckErr(err)

	//drop test db
	defer func() {
		err = testConn.Close()
		util.CheckErr(err)
		// can't drop db you're connected to
		conf.Set("database.db_name", "postgres")
		conn2 := OpenTestConn(conf, t)
		_, err = conn2.Exec(fmt.Sprintf("DROP DATABASE %s", testSchemaDbName))
		util.CheckErr(err)
	}()

	if len(testFuncs) == 0 {
		t.Fatal("no test functions set")
	}
	for _, f := range testFuncs {
		t.Run(f.name, f.testFunction)
	}

}
