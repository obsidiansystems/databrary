package routes

import (
	"net/http"
	"fmt"
	"github.com/databrary/databrary/services/sessions"
	"github.com/databrary/databrary/logging"
	gorilla_ses "github.com/gorilla/sessions"
)

func GetLogin(w http.ResponseWriter, r *http.Request) {
	var (
		sess *gorilla_ses.Session
		err  error
	)
	if sess, err = sessions.GetStore().Get(r, "databrary"); err != nil {
		logging.LogAndErrorf("couldn't get session from store: %+v", err)
	}
	if !sess.IsNew {
		w.Write([]byte("already logged in\n"))
		w.Write([]byte(fmt.Sprint(sess.Values["user_id"], "\n", sess.Values["user_email"])))
		return
	}
	w.Write([]byte(`<html>
		<form method="POST" action="">
    			<input type="text" name="email" placeholder="email">
    			<input type="password" name="password" placeholder="password">
    			<input type="submit" value="Submit">
			</form>
		</html>`))
}

func GetProfile(w http.ResponseWriter, r *http.Request) {
	sess, err := sessions.GetStore().Get(r, "databrary")
	if err != nil {
		fmt.Println("addf", err)
	}
	if !sess.IsNew {
		fmt.Println("session not new")
		w.Write([]byte(fmt.Sprint(sess.Values["user_id"], "\n", sess.Values["user_email"])))
		//w.Write(js)
		return
	}
}
