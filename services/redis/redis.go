package redis

import (
	"github.com/databrary/databrary/util"
	"github.com/garyburd/redigo/redis"
	"github.com/pkg/errors"
	"github.com/spf13/viper"
	"time"
)

var pool *redis.Pool

func InitPool(conf *viper.Viper) {
	pool = &redis.Pool{
		MaxIdle: 10,
		Dial: func() (redis.Conn, error) {
			return redis.Dial(
				"tcp",
				conf.GetString("redis.address"),
				redis.DialPassword(conf.GetString("redis.password")),
			)
		},
	}
}

func GetPool(conf *viper.Viper) (*redis.Pool, error) {
	if pool != nil {
		return pool, nil
	} else {
		return nil, errors.New("uninited redis pool")
	}

}

func GetConn() (redis.Conn, error) {
	if pool == nil {
		return nil, errors.New("uninited redis pool")
	}
	return pool.Get(), nil
}

// Save adds a session token and data to the RedisStore instance with the given expiry time.
// If the session token already exists then the data and expiry time are updated.
func Save(token string, b []byte, expiry time.Time) error {
	if pool == nil {
		return errors.New("uninited redis pool")
	}
	conn := pool.Get()
	defer conn.Close()

	err := conn.Send("MULTI")
	if err != nil {
		return err
	}
	err = conn.Send("SET", token, b)
	if err != nil {
		return err
	}
	err = conn.Send("PEXPIREAT", token, util.MakeMillisecondTimestamp(expiry))
	if err != nil {
		return err
	}
	_, err = conn.Do("EXEC")
	return err
}

func Find(token string) (b []byte, exists bool, err error) {
	if pool == nil {
		return []byte(""), false, errors.New("uninited redis pool")
	}
	conn := pool.Get()
	defer conn.Close()

	b, err = redis.Bytes(conn.Do("GET", token))
	if err == redis.ErrNil {
		return nil, false, nil
	} else if err != nil {
		return nil, false, err
	}
	return b, true, nil
}

func Delete(token string) error {
	if pool == nil {
		return errors.New("uninited redis pool")
	}
	conn := pool.Get()
	defer conn.Close()

	_, err := conn.Do("DEL", token)
	return err
}
