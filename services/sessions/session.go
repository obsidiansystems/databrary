package sessions

import (
	"net/http"

	"github.com/alexedwards/scs/engine/redisstore"
	"github.com/alexedwards/scs/session"
	"time"
	"github.com/databrary/databrary/util"
)

func NewSessionManager() func(http.Handler) http.Handler {
	session.ContextName = "databrary.session"
	session.CookieName = "databrary.session.token"
	engine := redisstore.New(NewRedisPool())
	return session.Manage(engine,
		// IdleTimeout sets the maximum length of time a session can be inactive
		// before it expires. By default IdleTimeout is not set (i.e. there is
		// no inactivity timeout).
		//session.IdleTimeout(30*time.Minute),

		// Lifetime sets the maximum length of time that a session is valid for
		// before it expires. This is an 'absolute expiry' and is set when the
		// session is first created. The default value is 24 hours.
		session.Lifetime(7*24*time.Hour),

		// Persist sets whether the session cookie should be persistent or not
		// (i.e. whether it should be retained after a user closes their browser).
		// The default value is false.
		session.Persist(true),
		//session.Domain("example.org"),  // Domain is not set by default.
		session.HttpOnly(false), // HttpOnly attribute is true by default.
		//session.Path("/account"),       // Path is set to "/" by default.
		//session.Secure(true),           // Secure attribute is false by default.
		session.ErrorFunc(SessionError),
	)
}

func SessionError(w http.ResponseWriter, r *http.Request, err error) {
	util.JsonErrorResponse(w, http.StatusInternalServerError, err, "session error")
}