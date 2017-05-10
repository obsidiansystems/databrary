package routes

import (
	"net/http"

	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	g_sessions "github.com/gorilla/sessions"
	"github.com/vattle/sqlboiler/queries/qm"
	"golang.org/x/crypto/bcrypt"

	"context"
	"fmt"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/sessions"
	"github.com/jmoiron/sqlx"
)

func PostLogin(w http.ResponseWriter, r *http.Request) {
	var (
		gSess *g_sessions.Session
		sess  sessions.Session
		err   error
		conn  *sqlx.DB
		ac    public_models.AccountSlice
	)

	// check if already logged in
	if sess, gSess, err = getSession(r); err != nil || gSess.IsNew {
		jsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't find session")
		return
	}
	if gSess.Values["LoggedIn"] == true || sess.LoggedIn {
		jsonErrorResponse(w, http.StatusNotModified, err, "already logged in")
		return
	}

	if err = r.ParseForm(); err != nil {
		jsonErrorResponse(w, http.StatusBadRequest, err, "couldn't parse form")
		return
	}
	if conn, err = db.OpenConn(config.GetConf()); err != nil {
		jsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't open db conn")
		return
	}
	if len(r.PostForm["email"]) != 1 || len(r.PostForm["password"]) != 1 {
		jsonErrorResponse(w, http.StatusBadRequest, err, "malformed email or password")
		return
	}
	email, pw, redirect := r.PostForm["email"][0], r.PostForm["password"][0], r.PostForm["redirect"][0]
	qrm := qm.Where("email = $1", email)
	if ac, err = public_models.Accounts(conn, qrm).All(); err != nil || len(ac) != 1 {
		jsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't find account %s", email)
		return
	}
	userAc := ac[0]
	// match is nil when pws match
	match := bcrypt.CompareHashAndPassword([]byte(userAc.Password.String), []byte(pw))
	if match != nil {
		jsonErrorResponse(w, http.StatusUnauthorized, err,
			"wrong password: user %#v ip %#v", r.PostForm["email"][0], r.RemoteAddr)
		return
	}
	sess.LoggedIn = true
	sess.AccountID = userAc.ID
	sess.RedirectURI = redirect
	gSess.Values = sess.ToMap()
	err = gSess.Save(r, w)
	if err != nil {
		jsonErrorResponse(w, http.StatusBadRequest, err, "couldn't save session")
		return
	}
	if sess.RedirectURI != "" {
		red := sess.RedirectURI
		// clear redirect
		sess.RedirectURI = ""
		fmt.Println(red)
		// since we got here with a POST we need to use status code 303 in order to tell to use GET for next
		http.Redirect(w, r.WithContext(context.WithValue(r.Context(), "session", sess)), red, http.StatusSeeOther)
	}
}

func PostLogout(w http.ResponseWriter, r *http.Request) {
	var (
		gSess *g_sessions.Session
		sess  sessions.Session
		err   error
	)
	// check if already logged in
	if sess, gSess, err = getSession(r); err != nil || gSess.IsNew {
		jsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't find session")
		return
	}
	if gSess.Values["LoggedIn"] == false || !sess.LoggedIn {
		jsonErrorResponse(w, http.StatusNotModified, err, "already logged out")
		return
	}
	sess.AccountID = -1
	sess.LoggedIn = false
	gSess.Values = sess.ToMap()
	err = gSess.Save(r, w)
	if err != nil {
		jsonErrorResponse(w, http.StatusConflict, err, "couldn't save session")
		return
	}
}

func IsLoggedIn(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		sess := r.Context().Value("session").(sessions.Session)
		if sess.LoggedIn {
			next.ServeHTTP(w, r)
		} else {
			logging.LogAndErrorf("unloggedin access to: from:", r.RequestURI, r.RemoteAddr)
			GetLogin(w, r)
		}
	})
}
