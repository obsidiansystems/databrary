package routes

import (
	"github.com/databrary/databrary/logging"
	"github.com/pressly/chi"
	"net/http"
)

// A completely separate router for administrator routes
func Api() http.Handler {
	r := chi.NewRouter()
	r.Route("/user", user)
	return r
}

func user(r chi.Router) {
	rateLimiter, err := NewRateLimiter()
	if err != nil {
		logging.LogWrapAndFatal(err, "couldn't create rate limiter")
	}
	r.With(rateLimiter.RateLimit).Post("/login", PostLogin)
	r.With(rateLimiter.RateLimit).Get("/login", GetLogin)
	r.Post("/logout", PostLogout)
	r.Get("/logout", PostLogout) // TODO remove only should be post
}

//func adminRouter() http.Handler {
//	r := chi.NewRouter()
//	r.Use(AdminOnly)
//	r.Get("/", adminIndex)
//	r.Get("/accounts", adminListAccounts)
//	return r
//}
