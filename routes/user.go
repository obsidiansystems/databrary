package routes

import (
	"net/http"

	"encoding/json"
	"fmt"
	"time"

	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/mail"
	"github.com/databrary/databrary/services/redis"
	"github.com/databrary/databrary/util"
	"github.com/databrary/scs/session"
	"github.com/pkg/errors"
	"github.com/satori/go.uuid"
	"github.com/vattle/sqlboiler/queries/qm"
	"golang.org/x/crypto/bcrypt"
	"gopkg.in/nullbio/null.v6"
)

func clearSession(w http.ResponseWriter, r *http.Request) {
	session.Clear(r)
	session.Save(w, r)
}

func PostLogin(w http.ResponseWriter, r *http.Request) {
	var (
		err, match      error
		ac              *public_models.Account
		email, password string
		ok              bool
		signature       []byte
		qrm             qm.QueryMod
	)

	if email, password, ok = r.BasicAuth(); !ok {
		clearSession(w, r)
		logging.Logger.Error(IpWrapMsg(r, "couldn't parse basic auth"))
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	dbConn, err := db.GetDbConn()

	if err != nil {
		clearSession(w, r)
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't open db conn"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	qrm = qm.Where("email = $1", email)

	if ac, err = public_models.Accounts(dbConn, qrm).One(); err != nil {
		clearSession(w, r)
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't find account %#v", email))
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	// match is nil when pws match
	match = bcrypt.CompareHashAndPassword([]byte(ac.Password.String), []byte(password))

	if match != nil {
		clearSession(w, r)
		logging.LogAndWrapError(match, IpWrapMsg(r, "wrong password for email %#v", email))
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	err = session.RegenerateToken(r)

	if err != nil {
		clearSession(w, r)
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't regen token"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	// sign the session with the current pw - so that if password changes all sessions are invalidated
	signature, err = bcrypt.GenerateFromPassword([]byte(ac.Password.String), bcrypt.MinCost)

	if err != nil {
		clearSession(w, r)
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't gen signature"))
		w.WriteHeader(http.StatusInternalServerError)
	}

	err = session.PutBytes(r, "signature", signature)
	err = session.PutInt(r, "account_id", ac.ID)
	err = session.PutBool(r, "logged_in", true)
	err = session.Save(w, r)

	if err != nil {
		clearSession(w, r)
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't save session"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	logging.Logger.Info(IpWrapMsg(r, "logged in id %d", ac.ID))
	util.WriteJSONResp(w, "ok", "sucess")
}

func PostLogOut(w http.ResponseWriter, r *http.Request) {
	clearSession(w, r)
	util.WriteJSONResp(w, "ok", "success")
}

// don't do logging in here, do logging where called
func isLoggedIn(r *http.Request) (bool, int, error) {
	signature, err := session.GetBytes(r, "signature")
	accountId, err := session.GetInt(r, "account_id")
	// if fresh or clear token
	if signature == nil || accountId == 0 {
		return false, http.StatusUnauthorized, nil
	}

	dbConn, err := db.GetDbConn()

	if err != nil {
		return false, http.StatusInternalServerError, err
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
			logging.LogAndWrapError(err, IpWrapMsg(r, ""))
			clearSession(w, r)
			w.WriteHeader(statusCode)
		}
		if loggedIn {
			next.ServeHTTP(w, r)
		} else {
			logging.Logger.Info(IpWrapMsg(r, "unloggedin access to: %#v", r.RequestURI))
			clearSession(w, r)
			w.WriteHeader(statusCode)
		}
	})

}

func IsLoggedInEndpoint(w http.ResponseWriter, r *http.Request) {
	type loggedInPayload struct {
		LoggedIn bool `json:"logged_in"`
	}
	loggedIn, statusCode, err := isLoggedIn(r)
	w.WriteHeader(statusCode)
	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, ""))
		clearSession(w, r)
		w.WriteHeader(statusCode)
	} else {
		if !loggedIn {
			logging.Logger.Info(IpWrapMsg(r, "unloggedin access to: %#v", r.RequestURI))
			clearSession(w, r)
			w.WriteHeader(statusCode)
		}
		util.WriteJSONResp(w, "ok", loggedInPayload{loggedIn})
	}

}

// the first stage of the reset password process - the auth token generation and email send
func ResetPasswordEmail(w http.ResponseWriter, r *http.Request) {
	clearSession(w, r)
	data := struct {
		Email string `json:"data"`
	}{}
	err := json.NewDecoder(r.Body).Decode(&data)

	if err != nil {
		var body []byte
		r.Body.Read(body)
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't decode data from body %s", string(body)))
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	dbConn, err := db.GetDbConn()

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't open db conn"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	ac, err := public_models.Accounts(dbConn, qm.Where("data = $1", data.Email)).One()

	if err != nil {
		// always report data sent to avoid tipping off brute-force attackers
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't locate account with data %#v", data.Email))
		util.WriteJSONResp(w, "ok", "success")
		return
	}

	ac.Password = null.String{"", false}
	tx, err := dbConn.Begin()

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't begin db tx"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	err = ac.Update(tx, "password")

	if err != nil {
		tx.Rollback()
		logging.WrapErrorAndLogWarn(err, "couldn't erase pw in database for account %#v", ac)
	}

	err = tx.Commit()

	if err != nil {
		tx.Rollback()
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't commit tx"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	pa, err := public_models.Parties(dbConn, qm.Where("id = $1", ac.ID)).One()

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't find id %#v", ac.ID))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	newUuid := uuid.NewV4()
	b, err := bcrypt.GenerateFromPassword(newUuid.Bytes(), bcrypt.MinCost)

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't generate auth token"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	payload, err := json.Marshal(struct {
		AccountID int `json:"account_id"`
	}{ac.ID})

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't marshal payload with account id %#v", ac.ID))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	token := fmt.Sprintf("databrary.auth:%s", string(b))
	err = redis.Save(token, payload, time.Now().AddDate(0, 0, 1))

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't save auth token %s with paylod %s", token, string(payload)))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	toName := fmt.Sprintf("%s %s", pa.Prename.String, pa.Name)
	err = mail.SendPasswordRecovery(toName, ac.Email, newUuid.String())

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't send data to %#v with uuid %#v", ac.Email, newUuid))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	logging.Logger.Info(IpWrapMsg(r, "reset password data stage for %#v", ac.ID))
	util.WriteJSONResp(w, "ok", "success")
}

func ResetPasswordToken(w http.ResponseWriter, r *http.Request) {

	clearSession(w, r)
	data := struct {
		Token       string `json:"data"`
		NewPassword string `json:"password"`
	}{}

	err := json.NewDecoder(r.Body).Decode(&data)

	if err != nil {
		var body []byte
		r.Body.Read(body)
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't decode data %s", string(body)))
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	authUuid, err := uuid.FromString(data.Token)

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't decode uuid %s", data.Token))
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	b, err := bcrypt.GenerateFromPassword(authUuid.Bytes(), bcrypt.MinCost)
	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't generate auth data hash from uuid %#v", authUuid))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	redisToken := fmt.Sprintf("databrary.auth:%s", string(b))
	// reuse b
	b, exists, err := redis.Find(redisToken)

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't fetch redis data"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
	if !exists {
		logging.Logger.Warn(IpWrapMsg(r, "auth token expired %s", redisToken))
		w.WriteHeader(http.StatusForbidden)
		return
	}

	redisPayload := struct {
		AccountID int `json:"account_id"`
	}{}

	err = json.Unmarshal(b, &redisPayload)
	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't unmarshal redis payload from %s", string(b)))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	dbConn, err := db.GetDbConn()
	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't open db conn"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	ac, err := public_models.Accounts(dbConn, qm.Where("id = $1", redisPayload.AccountID)).One()

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't locate account with id %#v", redisPayload.AccountID))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	ac.Password = null.String{data.NewPassword, true}
	tx, err := dbConn.Begin()

	if err != nil {
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't begin db tx"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	err = ac.Update(tx, "password")

	if err != nil {
		tx.Rollback()
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't update password"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	err = tx.Commit()

	if err != nil {
		tx.Rollback()
		logging.LogAndWrapError(err, IpWrapMsg(r, "couldn't commit tx"))
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	pa, err := public_models.Parties(dbConn, qm.Where("id = $1", ac.ID)).One()

	if err != nil {
		logging.WrapErrorAndLogWarn(err, IpWrapMsg(r, "couldn't find party with id %d", ac.ID))
	}

	toName := fmt.Sprintf("%s %s", pa.Prename.String, pa.Name)
	err = mail.SendPasswordRecoveryConfirmation(toName, ac.Email)

	if err != nil {
		logging.WrapErrorAndLogWarn(err, IpWrapMsg(r, "couldn't send email", ac.Email))
	}

	err = redis.Delete(redisToken)

	if err != nil {
		logging.WrapErrorAndLogWarn(err, "couldn't remove data %s from redis", redisToken)
	}

	logging.LogAndInfof(IpWrapMsg(r, "reset password for account %d", ac.ID))
	util.WriteJSONResp(w, "ok", "success")
}
