package routes

import (
	"fmt"
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	"github.com/databrary/scs/session"
	"github.com/databrary/sqlboiler/queries/qm"
	"github.com/jmoiron/sqlx"
	"net/http"
)

func GetLogin(w http.ResponseWriter, r *http.Request) {
	// check if already logged in
	if loggedIn, err := session.GetBool(r, "logged_in"); loggedIn {
		nInfo := NetInfoLogEntry(r)
		_, errorUuid := log.EntryWrapErr(nInfo, err, "already logged in")
		util.JsonErrResp(w, http.StatusBadRequest, errorUuid)
		return
	}
	scheme, addr, port := config.GetConf().Get("address.scheme"), config.GetConf().GetString("address.domain"), config.GetConf().GetString("address.backend_port")
	w.Write([]byte(fmt.Sprintf(`<html>
			<head>
			<script
				src="https://code.jquery.com/jquery-3.2.1.min.js"
				integrity="sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4="
				crossorigin="anonymous">
			</script>
			<script>
				function make_base_auth(user, password) {
    				var tok = user + ':' + password;
   		 			var hash = btoa(tok);
				    return 'Basic ' + hash;
				}
				function submit(ev) {
					ev.preventDefault();
					var endpoint = '%s://%s:%s/api/user/login';
					$.ajax({
						type: "POST",
						url: endpoint,
						success: function (data) { document.write(data); },
						beforeSend: function (xhr) {
    					    xhr.setRequestHeader('Authorization', make_base_auth($('#email').val(), $('#password').val()));
					    },
					});
				}
				window.onload = function() {
					console.log('hello');
					$('#login-form').submit(submit);
				};
			</script>
			</head>
			<form method="POST" id="login-form">
					<input type="text" name="email" placeholder="email" id="email">
					<input type="password" name="password" placeholder="password" id="password">
					<input type="submit" value="Submit">
			</form>
		</html>`, scheme, addr, port)))
}

func GetProfile(w http.ResponseWriter, r *http.Request) {
	var (
		err       error
		conn      *sqlx.DB
		p         *public_models.Party
		accountId int
	)
	nInfo := NetInfoLogEntry(r)

	if conn, err = db.GetDbConn(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if accountId, err = session.GetInt(r, "account_id"); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	qrm := qm.Where("id = $1", accountId)

	if p, err = public_models.Parties(conn, qrm).One(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find party %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	util.WriteJSONResp(w, "ok", p)
}
