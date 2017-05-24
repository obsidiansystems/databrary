package routes

import (
	"net/http"

	"encoding/json"
	"fmt"
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
	"time"
)

func clearSession(w http.ResponseWriter, r *http.Request) {
	session.Clear(r)
	session.Save(w, r)
}

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
		return
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

	err = session.RegenerateToken(r)
	if err != nil {
		clearSession(w, r)
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't save session")
		return
	}
	signature, _ := bcrypt.GenerateFromPassword([]byte(ac.Password.String), bcrypt.MinCost)
	err = session.PutBytes(r, "signature", signature)
	err = session.PutInt(r, "account_id", ac.ID)
	err = session.PutBool(r, "logged_in", true)
	err = session.Save(w, r)
	if err != nil {
		clearSession(w, r)
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't save session")
		return
	}
	util.WriteJSONResp(w, "ok", "sucess")
}

func PostLogOut(w http.ResponseWriter, r *http.Request) {
	clearSession(w, r)
	util.WriteJSONResp(w, "ok", "success")
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
			clearSession(w, r)
			w.WriteHeader(statusCode)
		}
		if loggedIn {
			next.ServeHTTP(w, r)
		} else {
			logging.LogAndInfof("unloggedin access to: from:", r.RequestURI, r.RemoteAddr)
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
		logging.LogAndError(err.Error())
		clearSession(w, r)
		util.WriteJSONResp(w, "error", err.Error())
	} else {
		if !loggedIn {
			clearSession(w, r)
		}
		util.WriteJSONResp(
			w,
			"ok",
			loggedInPayload{loggedIn},
		)
	}

}

func ResetPasswordEmail(w http.ResponseWriter, r *http.Request) {
	clearSession(w, r)
	email := struct {
		Email string `json:"email"`
	}{}
	err := json.NewDecoder(r.Body).Decode(&email)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusBadRequest, err, "")
		return
	}
	dbConn, err := db.GetDbConn()
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't open db conn")
		return
	}
	ac, err := public_models.Accounts(dbConn, qm.Where("email = $1", email.Email)).One()
	if err != nil {
		// always report email sent to avoid tipping off brute-force attacks
		util.WriteJSONResp(w, "ok", "success")
		return
	}
	ac.Password = null.String{"", false}
	err = ac.Update(dbConn, "password")
	if err != nil {
		logging.LogWrapAndError(err, "couldn't erase pw in database")
	}
	pa, err := public_models.Parties(dbConn, qm.Where("id = $1", ac.ID)).One()
	if err != nil {
		// always report email sent to avoid tipping off brute-force attacks
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't find party")
		return
	}

	newUuid := uuid.NewV4()
	b, err := bcrypt.GenerateFromPassword(newUuid.Bytes(), bcrypt.MinCost)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't generate auth token")
		return
	}
	token := fmt.Sprintf("databrary.auth:%s", string(b))
	payload, err := json.Marshal(struct {
		AccountID int `json:"account_id"`
	}{ac.ID})
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't save auth token payload")
		return
	}
	err = redis.Save(token, payload, time.Now().AddDate(0, 0, 1))
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't save auth token")
		return
	}
	toName := fmt.Sprintf("%s %s", pa.Prename.String, pa.Name)
	err = mail.SendPasswordRecovery(toName, ac.Email, newUuid.String())
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't send email")
		return
	}
	util.WriteJSONResp(w, "ok", "success")
}

func ResetPasswordToken(w http.ResponseWriter, r *http.Request) {

	clearSession(w, r)
	token := struct {
		Token       string `json:"token"`
		NewPassword string `json:"password"`
	}{}
	err := json.NewDecoder(r.Body).Decode(&token)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusBadRequest, err, "couldn't decode token")
		return
	}
	authUuid, err := uuid.FromString(token.Token)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusBadRequest, err, "couldn't decode uuid")
		return
	}

	b, err := bcrypt.GenerateFromPassword(authUuid.Bytes(), bcrypt.MinCost)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't generate auth token hash")
		return
	}
	redisToken := fmt.Sprintf("databrary.auth:%s", string(b))
	b, exists, err := redis.Find(redisToken)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't fetch redis token")
		return
	}
	if !exists {
		util.JsonErrorResponse(w, http.StatusBadRequest, err, "token expired")
		return
	}

	redisPayload := struct {
		AccountID int `json:"account_id"`
	}{}
	err = json.Unmarshal(b, &redisPayload)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't unmarshal redis payload")
		return
	}
	err = redis.Delete(redisToken)
	if err != nil {
		logging.LogWrapAndError(err, "couldn't remove token from redis")
	}
	dbConn, err := db.GetDbConn()
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't open db conn")
		return
	}
	ac, err := public_models.Accounts(dbConn, qm.Where("id = $1", redisPayload.AccountID)).One()
	if err != nil {
		// always report email sent to avoid tipping off brute-force attacks
		util.WriteJSONResp(w, "ok", "success")
		return
	}
	ac.Password = null.String{token.NewPassword, true}
	err = ac.Update(dbConn, "password")
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't update password in db")
		return
	}
	pa, err := public_models.Parties(dbConn, qm.Where("id = $1", ac.ID)).One()
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't find party")
		return
	}
	toName := fmt.Sprintf("%s %s", pa.Prename.String, pa.Name)
	err = mail.SendPasswordRecoveryConfirmation(toName, ac.Email)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't send email")
		return
	}
	util.WriteJSONResp(w, "ok", "success")
}
