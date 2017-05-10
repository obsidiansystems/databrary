package routes

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/sessions"
	g_sessions "github.com/gorilla/sessions"
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

type JSONResponse struct {
	Status  string      `json:"status"`
	Payload interface{} `json:"payload"`
}

func WriteJSONResp(w http.ResponseWriter, status string, msg interface{}) error {
	resp := JSONResponse{
		status,
		msg,
	}
	j, _ := json.Marshal(resp)
	_, err := w.Write(j)
	return err
}

func jsonErrorResponse(w http.ResponseWriter, code int, err error, msgf string, args ...interface{}) {
	var msg string
	if len(args) > 0 {
		msg = fmt.Sprintf(msgf, args...)
	} else {
		msg = msgf
	}
	w.WriteHeader(code)
	WriteJSONResp(w, "error", logging.LogWrapAndError(err, msg).Error())
}

func getSession(r *http.Request) (sessions.Session, *g_sessions.Session, error) {
	sess := r.Context().Value("session").(sessions.Session)
	gSess, err := sessions.GetStore().Get(r, "databrary")
	return sess, gSess, err

}

func Session(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var (
			gSess *g_sessions.Session
			sess  sessions.Session
			err   error
		)
		// this returns a new session with name databrary if none exists

		if gSess, err = sessions.GetStore().Get(r, "databrary"); err != nil {
			jsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't get session")
			return
		}
		// session exists - sync session store and context
		if !gSess.IsNew {
			sess = sessions.Session{
				LoggedIn:  gSess.Values["LoggedIn"].(bool),
				ID:        gSess.ID,
				AccountID: gSess.Values["AccountID"].(int),
			}
			// new session - create cookie
		} else {
			sess = sessions.Session{
				LoggedIn: false,
				ID:       gSess.ID,
			}
			gSess.Options = &g_sessions.Options{
				HttpOnly: true,
				Path:     "/",
				MaxAge:   0, // TODO should be short
				//Secure: true TODO turn on
			}
			gSess.Values = sess.ToMap()
			err = gSess.Save(r, w)
			if err != nil {
				jsonErrorResponse(w, http.StatusConflict, err, "couldn't save session")
				return
			}
		}
		next.ServeHTTP(w, r.WithContext(context.WithValue(r.Context(), "session", sess)))
	})
}
