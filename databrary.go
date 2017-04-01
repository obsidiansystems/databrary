package main

import (
	"path/filepath"

	"github.com/databrary/databrary/config"
	log "github.com/databrary/databrary/logging"
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

	log.Logger.WithField("test", "test").Debug("dfadfadf")
}
