package main

import (
	"path/filepath"

	"gopkg.in/alecthomas/kingpin.v2"
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/logging"
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
	logging.InitLgr(config.InitConf(config_path))

}

func main() {
	logrs := logging.GetLgr()
	logrs.WithField("test", "test").Debug("dfadfadf")
}
