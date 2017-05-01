package logging

import (
	"errors"
	"fmt"
	"time"

	"github.com/Sirupsen/logrus"
	"github.com/lestrrat/go-file-rotatelogs"
	"github.com/rifflock/lfshook"
	"github.com/spf13/viper"
)

var Logger *logrus.Logger

func InitLgr(conf *viper.Viper) *logrus.Logger {
	if Logger != nil {
		return Logger
	}

	log_path := conf.GetString("log.path")
	log_level := conf.GetString("log.level")

	lvl, err := logrus.ParseLevel(log_level)
	if err != nil {
		panic("can't parse log level")
	}

	Logger = logrus.New()
	Logger.Level = lvl

	// roratelogs config
	writer := rotatelogs.New(
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
	Logger.Hooks.Add(hook)
	return Logger
}

func LogAndError(msg string) error {
	Logger.Error(msg)
	return errors.New(msg)
}

func LogAndErrorf(format string, args ...interface{}) error {
	Logger.Errorf(format, args...)
	msg := fmt.Sprintf(format, args...)
	return errors.New(msg)
}