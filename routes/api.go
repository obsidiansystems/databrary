package routes

import (
	"github.com/Sirupsen/logrus"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/mail"
	"github.com/pressly/chi"
	"gopkg.in/throttled/throttled.v2"
	"net/http"
)

var rateLimiter throttled.HTTPRateLimiter

func init() {
	var err error
	rateLimiter, err = NewRateLimiter()
	if err != nil {
		log.WrapErrLogFatal(err, "couldn't create rate limiter")
	}
}

// A completely separate router for administrator routes
func Api() http.Handler {
	r := chi.NewRouter()
	r.Route("/user", user)
	r.With(rateLimiter.RateLimit).Get("/loggedin", IsLoggedInEndpoint)
	r.With(rateLimiter.RateLimit).Get("/report-error", ReportError)
	return r
}

func NetInfoLogEntry(r *http.Request) *logrus.Entry {
	fields := logrus.Fields(map[string]interface{}{
		"ip":  r.RequestURI,
		"uri": r.RemoteAddr,
	})
	return log.Logger.WithFields(fields)
}

func user(r chi.Router) {
	r.With(rateLimiter.RateLimit).Post("/login", PostLogin)
	r.With(rateLimiter.RateLimit).Post("/logout", PostLogOut)
	r.With(rateLimiter.RateLimit).Get("/login", GetLogin)

	r.With(rateLimiter.RateLimit).Post("/reset-password/email", ResetPasswordEmail)
	r.With(rateLimiter.RateLimit).Post("/reset-password/token", ResetPasswordToken)

}

func ReportError(w http.ResponseWriter, r *http.Request) {
	body := []byte("")
	r.Body.Read(body)
	mail.SendEmail(string(body), "error", "admin@databrary.org")
}

//func adminRouter() http.Handler {
//	r := chi.NewRouter()
//	r.Use(AdminOnly)
//	r.Get("/", adminIndex)
//	r.Get("/accounts", adminListAccounts)
//	return r
//}
