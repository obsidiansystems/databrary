package routes

import (
	"encoding/json"
	"fmt"
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/services/sessions"
	"github.com/jmoiron/sqlx"
	"github.com/vattle/sqlboiler/queries/qm"
	"net/http"
)

func GetLogin(w http.ResponseWriter, r *http.Request) {
	var (
		sess sessions.Session
		err  error
	)
	sess = r.Context().Value("session").(sessions.Session)
	if sess.LoggedIn {
		jsonErrorResponse(w, http.StatusNotModified, err, "already logged in")
		return
	}
	w.Write([]byte(fmt.Sprintf(`<html>
		<form method="POST" action="/api/user/login">
    			<input type="text" name="email" placeholder="email">
    			<input type="password" name="password" placeholder="password">
    			<input type="hidden" name="redirect" value="%s">
    			<input type="submit" value="Submit">
			</form>
		</html>`, r.RequestURI)))
}

func GetProfile(w http.ResponseWriter, r *http.Request) {
	var (
		sess sessions.Session
		err  error
		conn *sqlx.DB
		p    public_models.PartySlice
	)
	sess = r.Context().Value("session").(sessions.Session)
	if conn, err = db.OpenConn(config.GetConf()); err != nil {
		jsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't open db conn")
		return
	}
	qrm := qm.Where("id = $1", sess.AccountID)
	if p, err = public_models.Parties(conn, qrm).All(); err != nil || len(p) != 1 {
		jsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't find account %d", sess.AccountID)
		return
	}
	userP := p[0]
	j, err := json.Marshal(userP)
	if err != nil {
		jsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't marshal account %d", sess.AccountID)
		return
	}
	WriteJSONResp(w, "ok", string(j))
}
