package sessions

import (
	"github.com/garyburd/redigo/redis"
	"github.com/databrary/databrary/config"
	"github.com/alexedwards/scs/engine/redisstore"
)

func NewRedisPool() *redis.Pool {
	redisstore.Prefix = "databrary:session:"
	conf := config.GetConf()
	passwordOption := redis.DialPassword(conf.GetString("redis.password"))
	pool := &redis.Pool{
		MaxIdle: 10,
		Dial: func() (redis.Conn, error) {
			return redis.Dial(
				"tcp",
				conf.GetString("redis.address"),
				passwordOption,
			)
		},
	}
	return pool
}