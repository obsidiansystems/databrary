package redis

import (
	"github.com/garyburd/redigo/redis"
	"github.com/pkg/errors"
	"github.com/spf13/viper"
	"time"
)

// RedisStore represents the currently configured session storage engine. It is essentially
// a wrapper around a Redigo connection pool.
type RedisStore struct {
	pool *redis.Pool
}

var redisStore *RedisStore

// New returns a new RedisStore instance. The pool parameter should be a pointer to a
// Redigo connection pool. See https://godoc.org/github.com/garyburd/redigo/redis#Pool.
func InitRedisStore(conf *viper.Viper) {
	pool := &redis.Pool{
		MaxIdle: 10,
		Dial: func() (redis.Conn, error) {
			return redis.Dial(
				"tcp",
				conf.GetString("redis.address"),
				redis.DialPassword(conf.GetString("redis.password")),
			)
		},
	}
	redisStore = &RedisStore{pool}
}

func GetRedisStore() (*RedisStore, error) {
	if redisStore != nil {
		return redisStore, nil
	}
	return nil, errors.New("uninited redis store")
}

// Find returns the data for a given session token from the RedisStore instance. If the session
// token is not found or is expired, the returned exists flag will be set to false.
func (r *RedisStore) Find(token string) (b []byte, exists bool, err error) {
	conn := r.pool.Get()
	defer conn.Close()

	b, err = redis.Bytes(conn.Do("GET", token))
	if err == redis.ErrNil {
		return nil, false, nil
	} else if err != nil {
		return nil, false, err
	}
	return b, true, nil
}

// Save adds a session token and data to the RedisStore instance with the given expiry time.
// If the session token already exists then the data and expiry time are updated.
func (r *RedisStore) Save(token string, b []byte, expiry time.Time) error {
	conn := r.pool.Get()
	defer conn.Close()

	err := conn.Send("MULTI")
	if err != nil {
		return err
	}
	err = conn.Send("SET", token, b)
	if err != nil {
		return err
	}
	err = conn.Send("PEXPIREAT", token, makeMillisecondTimestamp(expiry))
	if err != nil {
		return err
	}
	_, err = conn.Do("EXEC")
	return err
}

// Delete removes a session token and corresponding data from the ResisStore instance.
func (r *RedisStore) Delete(token string) error {
	conn := r.pool.Get()
	defer conn.Close()

	_, err := conn.Do("DEL", token)
	return err
}

func makeMillisecondTimestamp(t time.Time) int64 {
	return t.UnixNano() / (int64(time.Millisecond) / int64(time.Nanosecond))
}
