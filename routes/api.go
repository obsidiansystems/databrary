package routes

import (
	"fmt"
	"github.com/databrary/databrary/logging"
	"github.com/pressly/chi"
	"gopkg.in/throttled/throttled.v2"
	"net/http"
)

var rateLimiter throttled.HTTPRateLimiter

func init() {
	var err error
	rateLimiter, err = NewRateLimiter()
	if err != nil {
		logging.WrapErrorAndLogFatal(err, "couldn't create rate limiter")
	}
}

// A completely separate router for administrator routes
func Api() http.Handler {
	r := chi.NewRouter()
	r.Route("/user", user)
	r.With(rateLimiter.RateLimit).Get("/loggedin", IsLoggedInEndpoint)
	return r
}

func IpWrapMsg(r *http.Request, msgf string, args ...interface{}) string {
	var msg string
	if len(args) > 0 {
		msg = fmt.Sprintf(msgf, args...)
	} else {
		msg = msgf
	}
	return fmt.Sprintf("from ip %#v : %s", r.RemoteAddr, msg)
}

func user(r chi.Router) {
	r.With(rateLimiter.RateLimit).Post("/login", PostLogin)
	r.With(rateLimiter.RateLimit).Post("/logout", PostLogOut)
	r.With(rateLimiter.RateLimit).Get("/login", GetLogin)

	r.With(rateLimiter.RateLimit).Post("/reset-password/email", ResetPasswordEmail)
	r.With(rateLimiter.RateLimit).Post("/reset-password/token", ResetPasswordToken)

}

//func adminRouter() http.Handler {
//	r := chi.NewRouter()
//	r.Use(AdminOnly)
//	r.Get("/", adminIndex)
//	r.Get("/accounts", adminListAccounts)
//	return r
//}
