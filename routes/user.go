package routes

import (
	"net/http"

	"encoding/json"
	"fmt"
	"time"

	"crypto/sha256"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/mail"
	"github.com/databrary/databrary/services/redis"
	"github.com/databrary/databrary/util"
	"github.com/databrary/scs/session"
	"github.com/databrary/sqlboiler/queries/qm"
	"github.com/jmoiron/sqlx"
	"github.com/pkg/errors"
	"github.com/satori/go.uuid"
	"golang.org/x/crypto/bcrypt"
	"gopkg.in/nullbio/null.v6"
	"io/ioutil"
	"net/url"
)

// Login with credentials and get a cookie if user wants to be remembered.
// Cookie only has redis token - no actual data is stored in the cookie.
func PostLogin(w http.ResponseWriter, r *http.Request) {
	var (
		err, match      error
		ac              *public_models.Account
		email, password string
		ok              bool
		signature       []byte
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

	if ac, err = public_models.Accounts(dbConn, qm.Where("email = $1", email)).One(); err != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %#v", email)
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	// check if passwords match
	// match is nil when pws match
	match = bcrypt.CompareHashAndPassword([]byte(ac.Password.String), []byte(password))

	if match != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "wrong password for email %#v", email)
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	// regen token in case they have a stale token from something.
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
	err = session.PutInt(r, "accountId", ac.ID)
	err = session.PutBool(r, "loggedIn", true)
	// error here shouldn't interfere with the user
	// worst case their login won't persist (but that doesn't mean we shouldn't let them in
	// should probably log but i'm too lazy right now
	_ = session.SetPersist(r, data.RememberMe)
	err = session.Save(w, r)

	if err != nil {
		session.Destroy(w, r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't save session")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}
	nInfo.Info("logged in id ", ac.ID)
	util.WriteJSONResp(w, "ok", "success")
}

func PostLogOut(w http.ResponseWriter, r *http.Request) {
	session.Destroy(w, r)
	util.WriteJSONResp(w, "ok", "success")
}

// Helper function for checking whether a user is logged in.
// Since it's a helper it doesn't log errors but returns them.
// Don't do logging in here, do logging where called
func isLoggedIn(r *http.Request) (bool, int, error) {
	signature, err := session.GetBytes(r, "signature")
	accountId, err := session.GetInt(r, "accountId")
	if err != nil {
		return false, http.StatusInternalServerError, err
	}
	// if fresh clear token
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

	// check if cookie has expired due to password change
	match := bcrypt.CompareHashAndPassword(signature, []byte(ac.Password.String))

	if match != nil {
		return false, http.StatusForbidden, errors.New("signature match fail")
	}

	loggedIn, err := session.GetBool(r, "loggedIn")

	if err != nil {
		return false, http.StatusInternalServerError, errors.Wrap(err, "couldn't get loggedIn from session")
	}

	if !loggedIn {
		return false, http.StatusForbidden, nil
	}

	return true, http.StatusOK, nil
}

// Middleware handler for checking if user is logged. Should be stacked
// before sensitive routes.
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

// Logged in endpoint. For checking whether a cookie in user's cookie
// store is a "logged in" cookie and still valid (password hasn't been changed).
func IsLoggedInEndpoint(w http.ResponseWriter, r *http.Request) {
	type loggedInPayload struct {
		LoggedIn bool `json:"loggedIn"`
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

// The first stage of the reset password process - the auth token generation and email send.
func ResetPasswordEmail(w http.ResponseWriter, r *http.Request) {
	session.Destroy(w, r)
	data := struct {
		Email *string `json:"email"`
	}{}
	nInfo := NetInfoLogEntry(r)
	err := json.NewDecoder(r.Body).Decode(&data)

	if err != nil || data.Email == nil {
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

	ac, err := public_models.Accounts(dbConn, qm.Where("email = $1", *data.Email)).One()

	if err != nil {
		// always report data sent to avoid tipping off brute-force attackers
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't locate account with email %#v", *data.Email)
		_ = errorUuid
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	tx, err := dbConn.Begin()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't begin db tx")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	// clear password so that if someone has unauthorized access using
	// the old password they won't be able to login anymore
	ac.Password = null.String{"", false}
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

	token, err := createToken(*pa, "auth", time.Now().Add(24*time.Hour))

	toName := fmt.Sprintf("%s %s", pa.Prename.String, pa.Name)
	err = mail.SendPasswordRecovery(toName, ac.Email, token)

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't send data to %#v with uuid %#v", ac.Email, token)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	nInfo.Infof("reset password email stage for %#v", ac.ID)
	util.WriteJSONResp(w, "ok", "success")
}

// Check to see whether a password recovery token has expired.
func CheckTokenExpiryEndpoint(w http.ResponseWriter, r *http.Request) {
	data := struct {
		Token *string `json:"token"`
	}{}
	nInfo := NetInfoLogEntry(r)

	err := json.NewDecoder(r.Body).Decode(&data)

	if err != nil || data.Token == nil {
		body, _ := ioutil.ReadAll(r.Body)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode data from body %s", string(body))
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	authUuid, err := uuid.FromString(*data.Token)
	_, msg, code, err := checkTokenExpiry(authUuid.String())
	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, msg)
		util.JsonErrResp(w, code, errorUuid)
		return
	}

	if code == http.StatusForbidden {
		log.EntryWrapErrLogWarn(nInfo, nil, "auth token expired %s", authUuid.String())
		w.WriteHeader(code)
		return
	}

	util.WriteJSONResp(w, "ok", "success")
}

// Token expiry helper
func checkTokenExpiry(token string) ([]byte, string, int, error) {
	authUuid, err := uuid.FromString(token)

	if err != nil {
		return nil, fmt.Sprintf("couldn't decode uuid %s", token), http.StatusBadRequest, err
	}

	// redis token is sha of that stored in cookie
	b := sha256.Sum256(authUuid.Bytes())

	if err != nil {
		return nil, fmt.Sprintf("couldn't generate auth data hash from uuid %#v", authUuid), http.StatusInternalServerError, err
	}

	redisToken := fmt.Sprintf("databrary.auth:%x", string(b[:]))
	redisStore, err := redis.GetRedisStore()
	if err != nil {
		return nil, "couldn't open redis conn", http.StatusInternalServerError, err
	}

	authData, exists, err := redisStore.Find(redisToken)

	if err != nil {
		return nil, "couldn't fetch redis data", http.StatusInternalServerError, err
	}

	if !exists {
		return nil, fmt.Sprintf("auth token expired %s", redisToken), http.StatusForbidden, nil
	}
	return authData, redisToken, 0, nil
}

// The actual reset password step of the reset password flow (as opposed to send email step).
func ResetPasswordToken(w http.ResponseWriter, r *http.Request) {

	session.Destroy(w, r)
	data := struct {
		Token       *string `json:"token"`
		NewPassword *string `json:"password"`
	}{}
	nInfo := NetInfoLogEntry(r)

	err := json.NewDecoder(r.Body).Decode(&data)

	if err != nil || data.Token == nil || data.NewPassword == nil {
		body, _ := ioutil.ReadAll(r.Body)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode data from body %s", string(body))
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	authUuid, err := uuid.FromString(*data.Token)
	authData, redisToken, code, err := checkTokenExpiry(authUuid.String())
	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, redisToken)
		util.JsonErrResp(w, code, errorUuid)
		return
	}

	if code == http.StatusForbidden {
		log.EntryWrapErrLogWarn(nInfo, nil, "auth token expired %s", authUuid.String())
		w.WriteHeader(code)
		return
	}

	redisPayload := struct {
		AccountID int `json:"accountId"`
	}{}

	err = json.Unmarshal(authData, &redisPayload)
	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't unmarshal redis payload from %s", redisToken)
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

	newPasswordHash, err := bcrypt.GenerateFromPassword([]byte(*data.NewPassword), 12)

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
	redisStore, err := redis.GetRedisStore()

	if err != nil {
		log.EntryWrapErrLogWarn(nInfo, err, "couldn't open redis conn")
		return
	}

	err = redisStore.Delete(redisToken)

	if err != nil {
		log.EntryWrapErrLogWarn(nInfo, err, "couldn't remove data %s from redis", redisToken)
	}

	nInfo.Infof("reset password for account %d", ac.ID)
	util.WriteJSONResp(w, "ok", "success")
}

// Check if user exists. This is a query param route i.e. ?email=bob@aol.com
func UserExists(w http.ResponseWriter, r *http.Request) {
	nInfo := NetInfoLogEntry(r)
	returnAnswer := func(ans bool) {
		util.WriteJSONResp(w, "ok", struct {
			Exists bool `json:"exists"`
		}{ans},
		)
	}

	rUrl, err := url.ParseRequestURI(r.RequestURI)

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode params from url", r.RequestURI)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	values := rUrl.Query()
	email := values.Get("email")

	if email == "" {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "no email", r.RequestURI)
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	dbConn, err := db.GetDbConn()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	_, err = public_models.Accounts(dbConn, qm.Where("email = $1", email)).One()
	if err == nil {
		returnAnswer(true)
	} else {
		returnAnswer(false)
	}

}

// Registration endpoint.
func Register(w http.ResponseWriter, r *http.Request) {

	session.Destroy(w, r)
	data := struct {
		FirstName   *string `json:"firstName"`
		LastName    *string `json:"lastName"`
		Email       *string `json:"email"`
		Affiliation *string `json:"affiliation"`
		ORCID       *string `json:"orcid"`
		URL         *string `json:"homepage"`
	}{}
	nInfo := NetInfoLogEntry(r)

	err := json.NewDecoder(r.Body).Decode(&data)

	if err != nil || data.FirstName == nil || data.LastName == nil || data.Email == nil ||
		data.Affiliation == nil || data.ORCID == nil || data.URL == nil {
		body, _ := ioutil.ReadAll(r.Body)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode data from body %s", string(body))
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	dbConn, err := db.GetDbConn()
	tx, err := dbConn.Begin()
	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	newParty := public_models.Party{
		Name:        *data.LastName,
		Prename:     null.NewString(*data.FirstName, true),
		Affiliation: null.NewString(*data.Affiliation, true),
	}

	if len(*data.ORCID) > 0 {
		newParty.Orcid = null.NewString(*data.ORCID, true)
	}

	if len(*data.URL) > 0 {
		newParty.URL = null.NewString(*data.URL, true)
	}

	err = newParty.Insert(tx)

	if err != nil {
		tx.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't create party")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	newAccount := public_models.Account{
		ID:    newParty.ID,
		Email: *data.Email,
	}

	// only insert email value (let db increment id)
	err = newAccount.Insert(tx)

	if err != nil {
		tx.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't create account")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = tx.Commit()

	if err != nil {
		tx.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't create commit account/party creation transaction")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	token, err := createToken(newParty, "auth", time.Now().Add(168*time.Hour))

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, token)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	toName := fmt.Sprintf("%s %s", newParty.Prename.String, newParty.Name)
	err = mail.SendPasswordRegistration(toName, newAccount.Email, token)

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't send data to %#v with uuid %#v", newAccount.Email, token)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	nInfo.Infof("account confirmation for %#v", newParty.ID)
	util.WriteJSONResp(w, "ok", "success")
}

// For password redis token reset and registration
func createToken(party public_models.Party, redisContext string, expiration time.Time) (string, error) {
	newUuid := uuid.NewV4()
	uuidAsBytes := sha256.Sum256(newUuid.Bytes())

	token := fmt.Sprintf("databrary.%s:%x", redisContext, string(uuidAsBytes[:]))
	payload, err := json.Marshal(struct {
		AccountID int `json:"accountId"`
	}{party.ID})

	if err != nil {
		return fmt.Sprintf("couldn't marshal payload with account id %#v", party.ID), err
	}

	redisStore, err := redis.GetRedisStore()

	if err != nil {
		return "couldn't open redis conn", err
	}

	err = redisStore.Save(token, payload, expiration)

	if err != nil {
		return fmt.Sprintf("couldn't save registration token %s with paylod %s", token, string(payload)), err
	}

	return newUuid.String(), nil
}

func GetProfile(w http.ResponseWriter, r *http.Request) {
	var (
		err       error
		dbConn    *sqlx.DB
		p         *public_models.Party
		a         *public_models.Account
		accountId int
	)
	nInfo := NetInfoLogEntry(r)

	if dbConn, err = db.GetDbConn(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db dbConn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if accountId, err = session.GetInt(r, "accountId"); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	qrm := qm.Where("id = $1", accountId)

	if a, err = public_models.Accounts(dbConn, qrm).One(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if p, err = public_models.Parties(dbConn, qrm).One(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find party %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	util.WriteJSONResp(w, "ok", struct {
		FirstName   string `json:"firstName"`
		LastName    string `json:"lastName"`
		Email       string `json:"email"`
		Affiliation string `json:"affiliation"`
		ORCID       string `json:"orcid"`
		URL         string `json:"url"`
		AccountId   int    `json:"accountId"`
	}{
		p.Prename.String,
		p.Name,
		a.Email,
		p.Affiliation.String,
		p.Orcid.String,
		p.URL.String,
		a.ID,
	})
}

// Update profile route.
func PatchProfile(w http.ResponseWriter, request *http.Request) {

	var (
		err       error
		dbConn    *sqlx.DB
		p         *public_models.Party
		a         *public_models.Account
		accountId int
	)

	data := struct {
		FirstName   *string `json:"firstName"`
		LastName    *string `json:"lastName"`
		Email       *string `json:"email"`
		Affiliation *string `json:"affiliation"`
		ORCID       *string `json:"orcid"`
		URL         *string `json:"url"`
		AccountId   *int    `json:"accountId"`
	}{}

	nInfo := NetInfoLogEntry(request)

	if accountId, err = session.GetInt(request, "accountId"); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = json.NewDecoder(request.Body).Decode(&data)

	if err != nil || data.FirstName == nil || data.LastName == nil || data.Email == nil ||
		data.Affiliation == nil || data.ORCID == nil || data.URL == nil || data.AccountId == nil {
		body, _ := ioutil.ReadAll(request.Body)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode data from body %s", string(body))
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	if *data.AccountId != accountId {
		err = errors.New("session account id and received account id don't match")
		_, errorUuid := log.EntryWrapErr(nInfo, err, "%d %d", *data.AccountId, accountId)
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	if dbConn, err = db.GetDbConn(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db dbConn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	qrm := qm.Where("id = $1", accountId)

	if a, err = public_models.Accounts(dbConn, qrm).One(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if p, err = public_models.Parties(dbConn, qrm).One(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find party %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	p.Name = *data.LastName
	p.Prename = null.StringFrom(*data.FirstName)
	p.URL = null.StringFrom(*data.URL)
	p.Orcid = null.StringFrom(*data.ORCID)
	p.Affiliation = null.StringFrom(*data.Affiliation)
	a.Email = *data.Email

	transaction, err := dbConn.Begin()

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't begin db transaction")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = p.Update(transaction, "name", "prename", "orcid", "url", "affiliation")

	if err != nil {
		transaction.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't update profile")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = a.Update(transaction, "email")

	if err != nil {
		transaction.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't update profile")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	err = transaction.Commit()

	if err != nil {
		transaction.Rollback()
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't commit transaction")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	util.WriteJSONResp(w, "ok", p)
}

// Get affiliates of a user.
func GetAffiliates(w http.ResponseWriter, request *http.Request) {
	var (
		err            error
		dbConn         *sqlx.DB
		authorizations public_models.AuthorizeSlice
	)
	nInfo := NetInfoLogEntry(request)

	data := struct {
		AccountID *int `json:"accountId"`
	}{}

	err = json.NewDecoder(request.Body).Decode(&data)

	if err != nil || data.AccountID == nil {
		body, _ := ioutil.ReadAll(request.Body)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode data from body %s", string(body))
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	if dbConn, err = db.GetDbConn(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db dbConn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if authorizations, err = public_models.Authorizes(dbConn,
		qm.Where("parent = $1", *data.AccountID),
		qm.Load("Child"), //this is key for getting R to work. note the capital letter C.
		// this is the name of struct field corresponding to the relationship you want to eagerly load
	).All(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find volume accesses %d", data.AccountID)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	var returnData []struct {
		FirstName string `json:"firstName"`
		LastName  string `json:"lastName"`
		ID        int    `json:"id"`
	}

	for _, v := range authorizations {
		returnData = append(returnData, struct {
			FirstName string `json:"firstName"`
			LastName  string `json:"lastName"`
			ID        int    `json:"id"`
		}{FirstName: v.R.Child.Prename.String, LastName: v.R.Child.Name, ID: v.R.Child.ID})
	}

	util.WriteJSONResp(w, "ok", returnData)
}
