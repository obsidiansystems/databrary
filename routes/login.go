package routes

import (
	"net/http"

	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/vattle/sqlboiler/queries/qm"
	"golang.org/x/crypto/bcrypt"
	"github.com/databrary/databrary/logging"
	"github.com/jmoiron/sqlx"
	"github.com/alexedwards/scs/session"
	"github.com/databrary/databrary/util"
	"github.com/pkg/errors"
)

func PostLogin(w http.ResponseWriter, r *http.Request) {
	var (
		err             error
		conn            *sqlx.DB
		ac              *public_models.Account
		email, password string
		ok              bool
	)
	// check if already logged in
	if loggedIn, err := session.GetBool(r, "logged_in"); loggedIn {
		util.JsonErrorResponse(w, http.StatusNotModified, err, "already logged in")
		return
	}
	if email, password, ok = r.BasicAuth(); !ok {
		util.JsonErrorResponse(w, http.StatusBadRequest, errors.New("couldn't parse basic auth"), "")
		return
	}
	if conn, err = db.OpenConn(config.GetConf()); err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't open db conn")
		return
	}
	qrm := qm.Where("email = $1", email)
	if ac, err = public_models.Accounts(conn, qrm).One(); err != nil {
		util.JsonErrorResponse(w, http.StatusBadRequest, err, "couldn't find account %s", email)
		return
	}
	// match is nil when pws match
	match := bcrypt.CompareHashAndPassword([]byte(ac.Password.String), []byte(password))
	if match != nil {
		util.JsonErrorResponse(w, http.StatusUnauthorized, err,
			"wrong password: user %#v ip %#v", email, r.RemoteAddr)
		return
	}

	err = session.RegenerateToken(r)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't regen token")
		return
	}
	err = session.PutBool(r, "logged_in", true)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't save session")
		return
	}
	session.PutInt(r, "account_id", ac.ID)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't save session")
		return
	}
	util.WriteJSONResp(w, "ok", "logged in")
}

func PostLogout(w http.ResponseWriter, r *http.Request) {
	var err error
	// check if already logged out
	if loggedIn, err := session.GetBool(r, "logged_in"); !loggedIn {
		util.JsonErrorResponse(w, http.StatusNotModified, err, "already logged out")
		return
	}
	err = session.Renew(r)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't clear session")
		return
	}
	util.WriteJSONResp(w, "ok", "logged out")
}

func IsLoggedIn(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		loggedIn, err := session.GetBool(r, "logged_in")
		if err != nil {
			util.JsonErrorResponse(w, http.StatusInternalServerError, err, "session error")
			return
		}
		if loggedIn {
			next.ServeHTTP(w, r)
		} else {
			logging.LogAndErrorf("unloggedin access to: from:", r.RequestURI, r.RemoteAddr)
			http.Redirect(w, r, "/api/user/login", http.StatusSeeOther)
		}
	})
}
