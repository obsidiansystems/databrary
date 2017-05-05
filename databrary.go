package main

import (
	"database/sql"
	"fmt"
	"path/filepath"

	"github.com/databrary/databrary/config"
	models "github.com/databrary/databrary/db/models/sqlboiler_models"
	log "github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	_ "github.com/lib/pq"
	"github.com/vattle/sqlboiler/boil"
	. "github.com/vattle/sqlboiler/queries/qm"
	"gopkg.in/alecthomas/kingpin.v2"
)

var (
	config_path = kingpin.Flag("config", "Path to config file").
		Required().
		Short('c').
		String()
)

func init() {
	// cmd line flags
	kingpin.Version("0.0.0")
	kingpin.Parse()
	config_path, err := filepath.Abs(*config_path)
	if err != nil {
		panic("command line config file path error")
	}
	log.InitLgr(config.InitConf(config_path))

}

func main() {
	db, _ := sql.Open("postgres", "dbname=databrary port=5433 user=postgres password=mysecretpassword sslmode=disable")
	boil.SetDB(db)
	users, err := models.AccountsG(Load("ID")).All()
	util.CheckOrFatalErr(err)
	for _, u := range users {
		fmt.Println(u.R.ID)
	}
	log.Logger.WithField("test", "test").Debug("dfadfadf")
}
