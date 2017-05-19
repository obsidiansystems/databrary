package routes

import (
	"net/http"

	"github.com/databrary/scs/session"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	"github.com/pkg/errors"
	"github.com/vattle/sqlboiler/queries/qm"
	"golang.org/x/crypto/bcrypt"
)

type loggedInPayload struct{ LoggedIn bool `json:"logged_in"` }

func PostLogin(w http.ResponseWriter, r *http.Request) {
	var (
		err             error
		ac              *public_models.Account
		email, password string
		ok              bool
	)
	if email, password, ok = r.BasicAuth(); !ok {
		util.JsonErrorResponse(w, http.StatusBadRequest, errors.New("couldn't parse basic auth"), "")
		return
	}
	dbConn, err := db.GetDbConn()
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

	clearSession := func(err error) {
		session.Clear(r)
		session.Save(w, r)
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't save session")
	}

	err = session.RegenerateToken(r)
	if err != nil {
		clearSession(err)
		return
	}
	signature, _ := bcrypt.GenerateFromPassword([]byte(ac.Password.String), bcrypt.MinCost)
	err = session.PutBytes(r, "signature", signature)
	err = session.PutInt(r, "account_id", ac.ID)
	err = session.PutBool(r, "logged_in", true)
	err = session.Save(w, r)
	if err != nil {
		clearSession(err)
		return
	}
	util.WriteJSONResp(w, "ok", "")
}

func PostLogOut(w http.ResponseWriter, r *http.Request) {
	session.Clear(r)
	session.Save(w, r)
	util.WriteJSONResp(w, "ok", "")
}

func isLoggedIn(r *http.Request) (bool, int, error) {
	signature, err := session.GetBytes(r, "signature")
	accountId, err := session.GetInt(r, "account_id")
	// if fresh or clear token
	if signature == nil || accountId == 0 {
		return false, http.StatusUnauthorized, nil
	}
	dbConn, err := db.GetDbConn()
	if err != nil {
		return false, http.StatusInternalServerError, errors.Wrap(err, "couldn't open db conn")
	}
	ac := &public_models.Account{}
	if ac, err = public_models.Accounts(dbConn, qm.Where("id = $1", accountId)).One(); err != nil {
		return false, http.StatusBadRequest, errors.Wrapf(err, "couldn't find account id %d", ac.ID)
	}
	match := bcrypt.CompareHashAndPassword(signature, []byte(ac.Password.String))
	if match != nil {
		return false, http.StatusForbidden, errors.New("signature match fail")
	}

	loggedIn, err := session.GetBool(r, "logged_in")
	if err != nil {
		return false, http.StatusInternalServerError, errors.Wrap(err, "couldn't get logged_in from session")
	}
	if !loggedIn {
		return false, http.StatusForbidden, nil
	}
	return true, http.StatusOK, nil
}

func IsLoggedInHandler(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		loggedIn, statusCode, err := isLoggedIn(r)
		if err != nil {
			logging.LogAndError(err.Error())
			session.Clear(r)
			session.Save(w, r)
			w.WriteHeader(statusCode)
		}
		if loggedIn {
			next.ServeHTTP(w, r)
		} else {
			logging.LogAndInfof("unloggedin access to: from:", r.RequestURI, r.RemoteAddr)
			session.Clear(r)
			session.Save(w, r)
			w.WriteHeader(statusCode)
		}
	})

}

func IsLoggedInEndpoint(w http.ResponseWriter, r *http.Request) {
	loggedIn, statusCode, err := isLoggedIn(r)
	w.WriteHeader(statusCode)
	if err != nil {
		logging.LogAndError(err.Error())
		session.Clear(r)
		session.Save(w, r)
		util.WriteJSONResp(w, "error", err.Error())
	} else {
		if !loggedIn {
			session.Clear(r)
			session.Save(w, r)
		}
		util.WriteJSONResp(
			w,
			"ok",
			loggedInPayload{loggedIn},
		)
	}

}
