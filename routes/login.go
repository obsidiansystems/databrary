package routes

import (
	"net/http"

	"context"
	"github.com/alexedwards/scs/session"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	"github.com/pkg/errors"
	"github.com/vattle/sqlboiler/queries/qm"
	"golang.org/x/crypto/bcrypt"
)

func PostLogin(w http.ResponseWriter, r *http.Request) {
	var (
		err             error
		ac              *public_models.Account
		email, password string
		ok              bool
	)
	// check if already logged in
	if loggedIn, err := session.GetBool(r, "logged_in"); loggedIn {
		util.JsonErrorResponse(w, http.StatusAlreadyReported, err, "already logged in")
		return
	}
	if email, password, ok = r.BasicAuth(); !ok {
		util.JsonErrorResponse(w, http.StatusBadRequest, errors.New("couldn't parse basic auth"), "")
		return
	}
	dbConn, err := getDb(r)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't open db conn")
	}
	qrm := qm.Where("email = $1", email)
	if ac, err = public_models.Accounts(dbConn, qrm).One(); err != nil {
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

	clearSession := func() {
		session.Clear(r)
		session.Save(w, r)
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't save session")
	}

	err = session.RegenerateToken(r)
	if err != nil {
		clearSession()
		return
	}
	signature, _ := bcrypt.GenerateFromPassword([]byte(ac.Password.String), bcrypt.MinCost)
	err = session.PutBytes(r, "signature", signature)
	err = session.PutInt(r, "account_id", ac.ID)
	err = session.PutBool(r, "logged_in", true)
	err = session.Save(w, r) // this isn't necessary because it's a cookie store but in case we migrate
	if err != nil {
		clearSession()
		return
	}
	util.WriteJSONResp(w, "ok", "logged in")
}

func PostLogout(w http.ResponseWriter, r *http.Request) {
	var err error
	// check if already logged out
	if loggedIn, err := session.GetBool(r, "logged_in"); !loggedIn || err != nil {
		// just in case
		session.Clear(r)
		util.JsonErrorResponse(w, http.StatusAlreadyReported, err, "already logged out")
		return
	}
	err = session.Clear(r)
	err = session.Save(w, r)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't clear session")
		return
	}

	util.WriteJSONResp(w, "ok", "logged out")
}

func IsLoggedIn(next http.Handler) http.Handler {

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		writeNotLoggedIn := func() {
			logging.LogAndErrorf("unloggedin access to: from:", r.RequestURI, r.RemoteAddr)
			util.WriteJSONResp(w, "fail", "not logged in")
		}

		signature, err := session.GetBytes(r, "signature")
		accountId, err := session.GetInt(r, "account_id")
		if signature == nil || accountId == 0 || err != nil {
			writeNotLoggedIn()
			return
		}

		dbConn, err := getDb(r)
		if err != nil {
			util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't open db conn")
		}
		ac := &public_models.Account{}
		if ac, err = public_models.Accounts(dbConn, qm.Where("id = $1", accountId)).One(); err != nil {
			util.JsonErrorResponse(w, http.StatusBadRequest, err, "couldn't find account from cookie")
			return
		}
		pwHash, _ := bcrypt.GenerateFromPassword([]byte(ac.Password.String), bcrypt.MinCost)
		match := bcrypt.CompareHashAndPassword(pwHash, signature)
		if match != nil {
			writeNotLoggedIn()
			return
		} else {
			if loggedIn, err := session.GetBool(r, "logged_in"); loggedIn && err != nil {
				ctx := context.WithValue(r.Context(), "db_conn", dbConn)
				next.ServeHTTP(w, r.WithContext(ctx))
			} else {
				writeNotLoggedIn()
				return
			}
		}
	})
}
