package routes

import (
	"net/http"

	"encoding/json"
	"fmt"
	"time"

	"crypto/sha256"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	log "github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/mail"
	"github.com/databrary/databrary/services/redis"
	"github.com/databrary/databrary/util"
	"github.com/databrary/scs/session"
	"github.com/pkg/errors"
	"github.com/satori/go.uuid"
	"github.com/vattle/sqlboiler/queries/qm"
	"golang.org/x/crypto/bcrypt"
	"gopkg.in/nullbio/null.v6"
	"io/ioutil"
	"net/url"
)

func PostLogin(w http.ResponseWriter, r *http.Request) {
	var (
		err, match      error
		ac              *public_models.Account
		email, password string
		ok              bool
		signature       []byte
		qrm             qm.QueryMod
	)
	nInfo := NetInfoLogEntry(r)
	if email, password, ok = r.BasicAuth(); !ok {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, nil, "couldn't parse basic auth")
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	data := struct {
		RememberMe bool `json:"rememberMe"`
	}{}
	err = json.NewDecoder(r.Body).Decode(&data)

	if err != nil {
		log.EntryWrapErr(nInfo, err, "couldn't read rememberMe")
	}

	dbConn, err := db.GetDbConn()

	if err != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	qrm = qm.Where("email = $1", email)

	if ac, err = public_models.Accounts(dbConn, qrm).One(); err != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %#v", email)
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	// match is nil when pws match
	match = bcrypt.CompareHashAndPassword([]byte(ac.Password.String), []byte(password))

	if match != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "wrong password for email %#v", email)
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	err = session.RegenerateToken(r)

	if err != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't regen token")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	// sign the session with the current pw - so that if password changes all sessions are invalidated
	signature, err = bcrypt.GenerateFromPassword([]byte(ac.Password.String), bcrypt.MinCost)

	if err != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't gen signature")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
	}

	err = session.PutBytes(r, "signature", signature)
	err = session.PutInt(r, "account_id", ac.ID)
	err = session.PutBool(r, "logged_in", true)
	// error here shouldn't be interfere with the user
	_ = session.SetPersist(r, data.RememberMe)
	err = session.Save(w, r)

	if err != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't save session")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}
	nInfo.Info("logged in id %d", ac.ID)
	util.WriteJSONResp(w, "ok", "success")
}

func PostLogOut(w http.ResponseWriter, r *http.Request) {
	session.Destroy(w, r)
	util.WriteJSONResp(w, "ok", "success")
}

// don't do logging in here, do logging where called
func isLoggedIn(r *http.Request) (bool, int, error) {
	signature, err := session.GetBytes(r, "signature")
	accountId, err := session.GetInt(r, "account_id")
	if err != nil {
		return false, http.StatusInternalServerError, err
	}
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
		nInfo := NetInfoLogEntry(r)
		loggedIn, statusCode, err := isLoggedIn(r)
		if err != nil {
			session.Destroy(w, r)
			_, errorUuid := log.EntryWrapErr(nInfo, err, "login handler failed")
			util.JsonErrResp(w, statusCode, errorUuid)
			return
		}
		if loggedIn {

			next.ServeHTTP(w, r)
		} else {
			session.Destroy(w, r)
			nInfo.Info("unloggedin access")
			w.WriteHeader(statusCode)
		}
	})

}

func IsLoggedInEndpoint(w http.ResponseWriter, r *http.Request) {
	type loggedInPayload struct {
		LoggedIn bool `json:"logged_in"`
	}
	nInfo := NetInfoLogEntry(r)
	loggedIn, statusCode, err := isLoggedIn(r)
	if err != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "login endpoint failed")
		util.JsonErrResp(w, statusCode, errorUuid)
		return
	}
	if !loggedIn {
		session.Destroy(w, r)
	}
	w.WriteHeader(statusCode)
	util.WriteJSONResp(w, "ok", loggedInPayload{loggedIn})
}

// the first stage of the reset password process - the auth token generation and email send
func ResetPasswordEmail(w http.ResponseWriter, r *http.Request) {
	session.Destroy(w, r)
	data := struct {
		Email string `json:"email"`
	}{}
	nInfo := NetInfoLogEntry(r)
	err := json.NewDecoder(r.Body).Decode(&data)

	if err != nil {
		body, _ := ioutil.ReadAll(r.Body)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode data from body %s", string(body))
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	dbConn, err := db.GetDbConn()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	ac, err := public_models.Accounts(dbConn, qm.Where("email = $1", data.Email)).One()

	if err != nil {
		// always report data sent to avoid tipping off brute-force attackers
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't locate account with email %#v", data.Email)
		_ = errorUuid
		//util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		util.WriteJSONResp(w, "ok", "success") //TODO
		return
	}

	ac.Password = null.String{"", false}
	tx, err := dbConn.Begin()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't begin db tx")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = ac.Update(tx, "password")

	if err != nil {
		tx.Rollback()
		log.EntryWrapErrLogWarn(nInfo, err, "couldn't erase pw in database for account %#v", ac)
	}

	err = tx.Commit()

	if err != nil {
		tx.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't commit tx")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	pa, err := public_models.Parties(dbConn, qm.Where("id = $1", ac.ID)).One()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find id %#v", ac.ID)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	newUuid := uuid.NewV4()
	uuidAsBytes := sha256.Sum256(newUuid.Bytes())

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't generate auth token")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	token := fmt.Sprintf("databrary.auth:%x", string(uuidAsBytes[:]))
	fmt.Println(token)
	payload, err := json.Marshal(struct {
		AccountID int `json:"account_id"`
	}{ac.ID})

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't marshal payload with account id %#v", ac.ID)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	redisStore, err := redis.GetRedisStore()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open redis conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = redisStore.Save(token, payload, time.Now().Add(24*time.Hour))

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't save auth token %s with paylod %s", token, string(payload))
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	toName := fmt.Sprintf("%s %s", pa.Prename.String, pa.Name)
	err = mail.SendPasswordRecovery(toName, ac.Email, newUuid.String())

	if err != nil {
		redisStore.Delete(token)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't send data to %#v with uuid %#v", ac.Email, newUuid)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	nInfo.Infof("reset password email stage for %#v", ac.ID)
	util.WriteJSONResp(w, "ok", "success")
}

func ResetPasswordToken(w http.ResponseWriter, r *http.Request) {

	session.Destroy(w, r)
	data := struct {
		Token       string `json:"token"`
		NewPassword string `json:"password"`
	}{}
	nInfo := NetInfoLogEntry(r)

	err := json.NewDecoder(r.Body).Decode(&data)

	if err != nil {
		body, _ := ioutil.ReadAll(r.Body)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode data from body %s", string(body))
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	authUuid, err := uuid.FromString(data.Token)

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode uuid %s", data.Token)
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	b := sha256.Sum256(authUuid.Bytes())

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't generate auth data hash from uuid %#v", authUuid)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	redisToken := fmt.Sprintf("databrary.auth:%x", string(b[:]))
	// reuse b
	redisStore, err := redis.GetRedisStore()
	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open redis conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	authData, exists, err := redisStore.Find(redisToken)

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't fetch redis data")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if !exists {
		log.EntryWrapErrLogWarn(nInfo, nil, "auth token expired %s", redisToken)
		w.WriteHeader(http.StatusForbidden)
		return
	}

	redisPayload := struct {
		AccountID int `json:"account_id"`
	}{}

	err = json.Unmarshal(authData, &redisPayload)
	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't unmarshal redis payload from %s", string(authData[:]))
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	dbConn, err := db.GetDbConn()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	ac, err := public_models.Accounts(dbConn, qm.Where("id = $1", redisPayload.AccountID)).One()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't locate account with id %#v", redisPayload.AccountID)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	newPasswordHash, err := bcrypt.GenerateFromPassword([]byte(data.NewPassword), 12)

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't generate password hash")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	ac.Password = null.String{string(newPasswordHash), true}
	tx, err := dbConn.Begin()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't begin db tx")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = ac.Update(tx, "password")

	if err != nil {
		tx.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't update password")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = tx.Commit()

	if err != nil {
		tx.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't commit tx")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	pa, err := public_models.Parties(dbConn, qm.Where("id = $1", ac.ID)).One()

	if err != nil {
		log.EntryWrapErrLogWarn(nInfo, err, "couldn't find party with id %d", ac.ID)
	}

	toName := fmt.Sprintf("%s %s", pa.Prename.String, pa.Name)
	err = mail.SendPasswordRecoveryConfirmation(toName, ac.Email)

	if err != nil {
		log.EntryWrapErrLogWarn(nInfo, err, "couldn't send email", ac.Email)
	}

	err = redisStore.Delete(redisToken)

	if err != nil {
		log.EntryWrapErrLogWarn(nInfo, err, "couldn't remove data %s from redis", redisToken)
	}

	nInfo.Infof("reset password for account %d", ac.ID)
	util.WriteJSONResp(w, "ok", "success")
}

func UserExists(w http.ResponseWriter, r *http.Request) {
	nInfo := NetInfoLogEntry(r)
	returnFalse := func() {
		util.WriteJSONResp(w, "ok", struct {
			Exists bool `json:"exists"`
		}{false},
		)
	}

	rUrl, err := url.ParseRequestURI(r.RequestURI)

	if err != nil {
		log.EntryWrapErr(nInfo, err, "couldn't decode params from url", r.RequestURI)
		returnFalse()
		return
	}

	values := rUrl.Query()
	email := values.Get("email")

	if email == "" {
		log.EntryWrapErr(nInfo, err, "no email", r.RequestURI)
		returnFalse()
		return
	}

	dbConn, err := db.GetDbConn()

	if err != nil {
		log.EntryWrapErr(nInfo, err, "couldn't open db conn")
		returnFalse()
		return
	}

	_, err = public_models.Accounts(dbConn, qm.Where("email = $1", email)).One()

	if err == nil {
		util.WriteJSONResp(w, "ok", struct {
			Exists bool `json:"exists"`
		}{true},
		)
	} else {
		returnFalse()
	}
}
