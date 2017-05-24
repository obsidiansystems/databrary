package sessions

import (
	"net/http"

	"time"

	"github.com/databrary/databrary/services/redis"
	"github.com/databrary/databrary/util"
	"github.com/databrary/scs/session"
)

var (
	ContextName = "databrary.session"
	CookieName  = "databrary.session.token"
)

func NewSessionManager() func(http.Handler) http.Handler {
	session.ContextName = ContextName
	session.CookieName = CookieName

	redisEngine, err := redis.GetRedisStore()
	if err != nil {
		panic(err.Error())
	}
	return session.Manage(redisEngine,
		session.Lifetime(7*24*time.Hour),
		session.Persist(false),
		//session.Domain("example.org"),  // Domain is not set by default.
		session.HttpOnly(true), // HttpOnly attribute is true by default.
		//session.Path("/account"),       // Path is set to "/" by default.
		session.Secure(false), //TODO // Secure attribute is false by default.
		session.ErrorFunc(SessionError),
	)
}

func SessionError(w http.ResponseWriter, r *http.Request, err error) {
	util.JsonErrorResponse(w, http.StatusInternalServerError, err, "session error")
}
