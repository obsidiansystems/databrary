package sessions

import (
	"fmt"
	"github.com/boj/redistore"
	"github.com/gorilla/sessions"
	"github.com/spf13/viper"
)

var store sessions.Store

func InitStore(conf *viper.Viper) error {
	var err error
	store, err = redistore.NewRediStore(
		10,
		"tcp",
		fmt.Sprintf(":%s", conf.GetString("redis.port")),
		conf.GetString("redis.password"),
		[]byte(conf.GetString("secret")))
	return err
}

func GetStore() sessions.Store {
	return store
}
