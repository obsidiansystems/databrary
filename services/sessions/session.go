package sessions

import (
	"net/http"

	"github.com/databrary/scs/engine/redisstore"
	"github.com/garyburd/redigo/redis"

	"time"

	"github.com/databrary/databrary/util"
	"github.com/databrary/scs/session"
	"github.com/databrary/databrary/config"
)

var (
	ContextName = "databrary.session"
	CookieName  = "databrary.session.token"
	Prefix      = ContextName
)

func NewSessionManager() func(http.Handler) http.Handler {
	conf := config.GetConf()
	session.ContextName = ContextName
	session.CookieName = CookieName
	redisstore.Prefix = Prefix + ":"
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
	engine := redisstore.New(pool)
	return session.Manage(engine,
		session.Lifetime(7*24*time.Hour),
		session.Persist(false),
		//session.Domain("example.org"),  // Domain is not set by default.
		session.HttpOnly(true), // HttpOnly attribute is true by default.
		//session.Path("/account"),       // Path is set to "/" by default.
		session.Secure(true), // Secure attribute is false by default.
		session.ErrorFunc(SessionError),
	)
}

func SessionError(w http.ResponseWriter, r *http.Request, err error) {
	util.JsonErrorResponse(w, http.StatusInternalServerError, err, "session error")
}
