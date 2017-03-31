package logging


import (
	"io/ioutil"
	"time"

	logrus "github.com/Sirupsen/logrus"
	"github.com/lestrrat/go-file-rotatelogs"
	"github.com/rifflock/lfshook"
	"github.com/spf13/viper"
)


var log *logrus.Logger


func InitLgr(conf *viper.Viper) *logrus.Logger {
	if log != nil {
		return log
	}

	log_path := conf.GetString("log.path")
	log_level := conf.GetString("log.level")

	lvl, err := logrus.ParseLevel(log_level)
	if err != nil {
		panic("can't parse log level")
	}

	log = logrus.New()
	log.Level = lvl
	log.Out = ioutil.Discard
	logrus.SetOutput(ioutil.Discard)

	// roratelogs config
	writer, err := rotatelogs.New(
		log_path+".%Y%m%d%H%M", // rotation pattern
		rotatelogs.WithLinkName(log_path),
		rotatelogs.WithRotationTime(time.Duration(86400)*time.Second), // rotate once a day
		rotatelogs.WithMaxAge(time.Duration(604800)*time.Second),      // keep one week of log files
	)
	if err != nil {
		panic("couldn't create rotate logs writer")
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
	log.Hooks.Add(hook)
	return log
}


func GetLgr() *logrus.Logger {
	if log == nil {
		panic("tried to get uninited logger")
	}
	return log
}