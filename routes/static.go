package routes

import (
	"encoding/json"
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/jmoiron/sqlx"
	"github.com/vattle/sqlboiler/queries/qm"
	"net/http"
	"github.com/alexedwards/scs/session"
	"github.com/databrary/databrary/util"
)

func GetLogin(w http.ResponseWriter, r *http.Request) {
	// check if already logged in
	if loggedIn, err := session.GetBool(r, "logged_in"); loggedIn {
		util.JsonErrorResponse(w, http.StatusNotModified, err, "already logged in")
		return
	}
	w.Write([]byte(`<html>
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
					var endpoint = 'http://localhost:3444/api/user/login';
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
		</html>`))
}

func GetProfile(w http.ResponseWriter, r *http.Request) {
	var (
		err       error
		conn      *sqlx.DB
		p         *public_models.Party
		accountId int
	)

	if conn, err = db.OpenConn(config.GetConf()); err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't open db conn")
		return
	}
	if accountId, err = session.GetInt(r, "account_id"); err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't find account %d", accountId)
		return
	}
	qrm := qm.Where("id = $1", accountId)
	if p, err = public_models.Parties(conn, qrm).One(); err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't find party %d", accountId)
		return
	}
	j, err := json.Marshal(p)
	if err != nil {
		util.JsonErrorResponse(w, http.StatusInternalServerError, err, "couldn't marshal account %d", accountId)
		return
	}
	util.WriteJSONResp(w, "ok", string(j))
}
