package routes

import (
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	"github.com/databrary/sqlboiler/queries/qm"
	"github.com/jmoiron/sqlx"
	"github.com/pressly/chi"
	"net/http"
	"encoding/json"
	"io/ioutil"
	"github.com/databrary/scs/session"
	"errors"
)

func volume(r chi.Router) {
	r.Get("/all", GetUserVolumes)
	r.With(IsLoggedInHandler).Group(func(r chi.Router) {

	})
}

func GetUserVolumes(w http.ResponseWriter, request *http.Request) {
	var (
		err     error
		dbConn  *sqlx.DB
		volumes public_models.VolumeSlice
	)
	nInfo := NetInfoLogEntry(request)

	data := struct {
		AccountID *int `json:"accountId"`
		Perm      *string `json:"perm"`
	}{}

	err = json.NewDecoder(request.Body).Decode(&data)

	if err != nil || data.AccountID == nil || data.Perm == nil {
		body, _ := ioutil.ReadAll(request.Body)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't decode data from body %s", string(body))
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}

	isLoggedIn, statusCode, err := isLoggedIn(request)

	if err != nil {
		session.Destroy(w, request)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "login handler failed")
		util.JsonErrResp(w, statusCode, errorUuid)
		return
	} else if isLoggedIn {
		if accountId, err := session.GetInt(request, "accountId"); err != nil {
			_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %d", accountId)
			util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
			return
		} else {
			if accountId != *data.AccountID {
				err = errors.New("session account id and received account id don't match")
				_, errorUuid := log.EntryWrapErr(nInfo, err, "%d %d", *data.AccountID, accountId)
				util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
				return
			}
		}
	} else { // !isLoggedIn
		// this is just to avoid a nil pointer deref (can't assign directly to *data.AccountID because it doesn't
		// have a memory location
		dummy1 := -1
		dummy2 := public_models.PermissionPUBLIC
		data.AccountID = &dummy1
		data.Perm = &dummy2
	}

	if dbConn, err = db.GetDbConn(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db dbConn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if volumes, err = public_models.Volumes(dbConn,
		qm.InnerJoin("volume_access_view on volume.id = volume"),
		qm.Where("access <= $1", *data.Perm),
		qm.And("party = $2", *data.AccountID),
	).All(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find volume accesses %d", *data.AccountID)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	util.WriteJSONResp(w, "ok", volumes)
}
