package main



import (
	"fmt"
	"path/filepath"
	"io/ioutil"
	"time"
	"log"

	logrus "github.com/Sirupsen/logrus"
	"gopkg.in/alecthomas/kingpin.v2"
	"github.com/spf13/viper"
	"github.com/lestrrat/go-file-rotatelogs"
	"github.com/rifflock/lfshook"
)

var (
	debug	= kingpin.Flag("debug", "Enable debug mode.").Bool()
	config_path  = kingpin.Flag("config", "Path to config file").
		Required().
		Short('c').
		String()
)

var (
	// this should imported anywhere you need logging
	// instead of the logrus package because of the custom rotatelogs
	// hooks
	Logrs *logrus.Logger
)

func init() {
	// cmd line flags
	kingpin.Version("0.0.0")
	kingpin.Parse()
	config_path, err := filepath.Abs(*config_path)
	if err != nil {
		panic("command line config file path error")
	}

	// viper config
	viper.SetConfigFile(config_path)
	err = viper.ReadInConfig() // Find and read the config file
	if err != nil {                 // Handle errors reading the config file
		panic(fmt.Sprintf("config file error: %s", err))
	} else {
		fmt.Printf("config %s successfully loaded\n", viper.GetViper().ConfigFileUsed())
	}
	conf := viper.GetViper()

	// logrus config
	log_path := conf.GetString("log_path")
	log_level := conf.GetString("log_level")

	lvl, err := logrus.ParseLevel(log_level)
	if err != nil {
		panic("can't parse log level")
	}

	Logrs = logrus.New()
	Logrs.Level = lvl
	Logrs.Out = ioutil.Discard
	logrus.SetOutput(ioutil.Discard)

	// roratelogs config
	writer, err := rotatelogs.New(
		log_path+".%Y%m%d%H%M", // rotation pattern
		rotatelogs.WithLinkName(log_path),
		rotatelogs.WithRotationTime(time.Duration(86400)*time.Second), // rotate once a day
		rotatelogs.WithMaxAge(time.Duration(604800)*time.Second),      // keep one week of log files
	)
	if err != nil {
		log.Fatalln("couldn't create rotate logs")
	}

	hook := lfshook.NewHook(lfshook.WriterMap{
		logrus.DebugLevel: writer,
		logrus.FatalLevel: writer,
		logrus.PanicLevel: writer,
		logrus.WarnLevel:  writer,
		logrus.InfoLevel:  writer,
		logrus.ErrorLevel: writer,
	})
	hook.SetFormatter(&logrus.JSONFormatter{})
	Logrs.Hooks.Add(hook)
}


func main() {
	Logrs.WithField("test", "test").Debug("dfadfadf")
}
