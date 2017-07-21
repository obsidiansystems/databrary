package routes

import (
	"encoding/json"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	"github.com/databrary/scs/session"
	"github.com/databrary/sqlboiler/queries/qm"
	"github.com/jmoiron/sqlx"
	"github.com/pressly/chi"
	"io/ioutil"
	"net/http"
)

func volume(r chi.Router) {
	r.Get("/all", GetUserVolumes)
	r.With(IsLoggedInHandler).Group(func(r chi.Router) {

	})
}

// Get a user's volumes. Right now you can only get your own volumes.
func GetUserVolumes(w http.ResponseWriter, request *http.Request) {
	var (
		err     error
		dbConn  *sqlx.DB
		volumes public_models.VolumeSlice
	)
	nInfo := NetInfoLogEntry(request)

	data := struct {
		AccountID *int    `json:"accountId"`
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

	//TODO: this is messed up. If a user is logged in then they should only
	//be able to get their own non-public volumes but anyone's public volumes
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
			//
			if accountId != *data.AccountID {
				accountId := -1
				if data.AccountID != nil {
					// if someone wants to look at the public volumes belonging to someone else
					accountId = *data.AccountID
				}
				// this is just to avoid a nil pointer deref (can't assign directly to *data.AccountID because it doesn't
				// have a memory location
				perm := public_models.PermissionPUBLIC
				data.AccountID = &accountId
				data.Perm = &perm
			}
		}
	} else { // !isLoggedIn
		accountId := -1
		if data.AccountID != nil {
			// if someone wants to look at the public volumes belonging to someone else
			accountId = *data.AccountID
		}
		// this is just to avoid a nil pointer deref (can't assign directly to *data.AccountID because it doesn't
		// have a memory location
		perm := public_models.PermissionPUBLIC
		data.AccountID = &accountId
		data.Perm = &perm
	}

	if dbConn, err = db.GetDbConn(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db dbConn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}
	// select * from volume join volume_access_view on volume.id = volume where access <= $1 and party = $2
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
