package routes

import (
	"fmt"
	"net/http"

	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/sessions"
	gorilla_ses "github.com/gorilla/sessions"
	"github.com/vattle/sqlboiler/queries/qm"
	"golang.org/x/crypto/bcrypt"

	"github.com/jmoiron/sqlx"
)

func PostLogin(w http.ResponseWriter, r *http.Request) {
	var (
		sess *gorilla_ses.Session
		err  error
		conn *sqlx.DB
		ac   public_models.AccountSlice
	)
	if sess, err = sessions.GetStore().Get(r, "databrary"); err != nil {
		logging.LogAndErrorf("couldn't get session from store: %+v", err)
	}
	if !sess.IsNew {
		w.Write([]byte(fmt.Sprint(sess.Values["user_id"], "\n", sess.Values["user_email"])))
		return
	}
	if err = r.ParseForm(); err != nil {
		logging.LogAndHTTPWriteErrorf(w, "couldn't parse login form: %+v", err)
		return
	}
	if conn, err = db.OpenConn(config.GetConf()); err != nil {
		logging.LogAndHTTPWriteErrorf(w, "couldn't open db conn: %+v", err)
		return
	}
	if len(r.PostForm["email"]) != 1 || len(r.PostForm["password"]) != 1 {
		logging.LogAndHTTPWriteErrorf(w, "couldn't malformed form params: %#v", r.PostForm)
		return
	}
	email, pw := r.PostForm["email"][0], r.PostForm["password"][0]
	qrm := qm.Where("email = $1", email)
	if ac, err = public_models.Accounts(conn, qrm).All(); err != nil || len(ac) != 1 {
		logging.LogAndHTTPWriteErrorf(w, "couldn't find account: %+v", err)
		return
	}
	userAc := ac[0]
	// match is nil when pws match
	if match := bcrypt.CompareHashAndPassword([]byte(userAc.Password.String), []byte(pw)); match == nil {

		if p, err := public_models.FindParty(conn, userAc.ID); err != nil {
			logging.LogAndHTTPWriteErrorf(w, "couldn't malformed form params: %#v", r.PostForm)
			return
		} else {
			sess := gorilla_ses.NewSession(sessions.GetStore(), "databrary")
			sess.Options = &gorilla_ses.Options{
				HttpOnly: true,
				//Secure:   true,
				MaxAge: 0,
			}
			sess.Values["user_id"] = p.ID
			sess.Values["user_email"] = userAc.Email
			err := sess.Save(r, w)
			if err != nil {
				fmt.Println("err", err)
			}
			w.Write([]byte("session saved"))
		}
	} else {
		logging.LogAndHTTPWriteErrorf(w, "wrong password: user %#v ip %#v", r.PostForm["email"][0], r.RemoteAddr)
		return
	}
}

func PostLogout(w http.ResponseWriter, r *http.Request) {
	var (
		sess *gorilla_ses.Session
		err  error
	)
	if sess, err = sessions.GetStore().Get(r, "databrary"); err != nil {
		logging.LogAndErrorf("couldn't get session from store: %+v", err)
		return
	}
	if !sess.IsNew {
		fmt.Println("logging out")
		sess.Options = &gorilla_ses.Options{
			HttpOnly: true,
			Secure:   true,
			MaxAge:   -1,
		}
		sess.Values["user_id"] = ""
		sess.Values["user_email"] = ""

		if err := sess.Save(r, w); err != nil {
			logging.LogAndErrorf("couldn't save session: %+v", err)
			return
		}
		w.Write([]byte("logged out"))
	} else {
		w.Write([]byte("already logged out"))
	}

}

func IfLoggedIn(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Context().Value("session_id") != nil {
			next.ServeHTTP(w, r)
			return
		}
		if sess, err := sessions.GetStore().Get(r, "databrary"); err != nil {
			logging.LogAndErrorf("couldn't get session from store: %+v", err)
		} else {
			if !sess.IsNew {
				next.ServeHTTP(w, r)
			} else {
				logging.LogAndErrorf("unauthorized access to: from:", r.RequestURI, r.RemoteAddr)
				http.Redirect(w, r, "/api/user/login", http.StatusForbidden)
			}
		}
	})
}
