package logging

import (
	logrus "github.com/Sirupsen/logrus"
	"github.com/databrary/databrary/config"
	"github.com/lestrrat/go-file-rotatelogs"
	"github.com/rifflock/lfshook"
	"io/ioutil"
	"log"
	"time"
)

type customLgCxt struct {
	ContextLog *logrus.Entry
}

var logrusLogger *logrus.Logger
var dbraryBaseLgCxt *customLgCxt = nil

// TODO: stupid hack because no aliases
type Fields map[string]interface{}

func MkLgrsFlds(f Fields) logrus.Fields {
	return (logrus.Fields)(f)
}

func InitLogger(conf config.Config) {
	if logrusLogger != nil {
		return
	}

	log_path := conf.GetStringEntry("log_path")
	log_level := conf.GetStringEntry("log_level")
	lvl, err := logrus.ParseLevel(log_level)
	if err != nil {
		panic("can't parse log level")
	}

	logrusLogger = logrus.New()

	logrusLogger.Level = lvl
	logrusLogger.Out = ioutil.Discard
	logrus.SetOutput(ioutil.Discard)

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
	logrusLogger.Hooks.Add(hook)
	dbraryBaseLgCxt = &customLgCxt{
		ContextLog: logrusLogger.WithFields(
			logrus.Fields{"app": "databrary"},
		),
	}
}

func GetDbryLgr() *customLgCxt {
	if dbraryBaseLgCxt == nil {
		log.Fatalln("get logger before init")
	}
	return dbraryBaseLgCxt
}
