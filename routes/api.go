package routes

import (
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
	r.Post("/login", PostLogin)
	r.Get("/login", GetLogin)
	//r.Post("/logout", routes.PostLogout)
	r.Get("/logout", PostLogout)
}

//func adminRouter() http.Handler {
//	r := chi.NewRouter()
//	r.Use(AdminOnly)
//	r.Get("/", adminIndex)
//	r.Get("/accounts", adminListAccounts)
//	return r
//}
