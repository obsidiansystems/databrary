package main

import (
	"path/filepath"

	"github.com/databrary/databrary/config"
	log "github.com/databrary/databrary/logging"
	"gopkg.in/alecthomas/kingpin.v2"
	"net/http"
	//"github.com/gorilla/mux"
	//routes "github.com/databrary/databrary/controllers/routes"
	//"time"
	"gopkg.in/authboss.v1"
	"os"

	"encoding/base64"
	"fmt"
	"html/template"
	"io"
	"time"

	_ "gopkg.in/authboss.v1/auth"
	_ "gopkg.in/authboss.v1/confirm"
	_ "gopkg.in/authboss.v1/lock"
	_ "gopkg.in/authboss.v1/recover"
	_ "gopkg.in/authboss.v1/register"
	//_ "gopkg.in/authboss.v1/remember"

	"github.com/gorilla/csrf"
	"github.com/gorilla/mux"
	"github.com/gorilla/securecookie"
	"github.com/gorilla/sessions"
)

var funcs = template.FuncMap{
	"formatDate": func(date time.Time) string {
		return date.Format("2006/01/02 03:04pm")
	},
	"yield": func() string { return "" },
}

var (
	ab       = authboss.New()
	database = NewMemStorer()
)

func setupAuthboss() {
	ab.Storer = database
	ab.MountPath = "/auth"
	ab.ViewsPath = "ab_views"
	ab.RootURL = `http://localhost:8080`

	//b, err := ioutil.ReadFile(filepath.Join("views", "layout.html.tpl"))
	//if err != nil {
	//	panic(err)
	//}
	//ab.Layout = template.Must(template.New("layout").Funcs(funcs).Parse(string(b)))

	ab.XSRFName = "csrf_token"
	ab.XSRFMaker = func(_ http.ResponseWriter, r *http.Request) string {
		return csrf.Token(r)
	}

	ab.CookieStoreMaker = NewCookieStorer
	ab.SessionStoreMaker = NewSessionStorer

	ab.Mailer = authboss.LogMailer(os.Stdout)

	ab.Policies = []authboss.Validator{
		authboss.Rules{
			FieldName:       "email",
			Required:        true,
			AllowWhitespace: false,
		},
		authboss.Rules{
			FieldName:       "password",
			Required:        true,
			MinLength:       4,
			MaxLength:       8,
			AllowWhitespace: false,
		},
	}

	if err := ab.Init(); err != nil {
		log.Logger.Fatal(err)
	}
}

var (
	config_path = kingpin.Flag("config", "Path to config file").
		Required().
		Short('c').
		String()
)

func init() {
	// cmd line flags
	kingpin.Version("0.0.0")
	kingpin.Parse()
	config_path, err := filepath.Abs(*config_path)
	if err != nil {
		panic("command line config file path error")
	}
	log.InitLgr(config.InitConf(config_path))

}

func main() {
	// Initialize Sessions and Cookies
	// Typically gorilla securecookie and sessions packages require
	// highly random secret keys that are not divulged to the public.
	//
	// In this example we use keys generated one time (if these keys ever become
	// compromised the gorilla libraries allow for key rotation, see gorilla docs)
	// The keys are 64-bytes as recommended for HMAC keys as per the gorilla docs.
	//
	// These values MUST be changed for any new project as these keys are already "compromised"
	// as they're in the public domain, if you do not change these your application will have a fairly
	// wide-opened security hole. You can generate your own with the code below, or using whatever method
	// you prefer:
	//
	//    func main() {
	//        fmt.Println(base64.StdEncoding.EncodeToString(securecookie.GenerateRandomKey(64)))
	//    }
	//
	// We store them in base64 in the example to make it easy if we wanted to move them later to
	// a configuration environment var or file.
	cookieStoreKey, _ := base64.StdEncoding.DecodeString(`NpEPi8pEjKVjLGJ6kYCS+VTCzi6BUuDzU0wrwXyf5uDPArtlofn2AG6aTMiPmN3C909rsEWMNqJqhIVPGP3Exg==`)
	sessionStoreKey, _ := base64.StdEncoding.DecodeString(`AbfYwmmt8UCwUuhd9qvfNA9UCuN1cVcKJN1ofbiky6xCyyBj20whe40rJa3Su0WOWLWcPpO1taqJdsEI/65+JA==`)
	cookieStore = securecookie.New(cookieStoreKey, nil)
	sessionStore = sessions.NewCookieStore(sessionStoreKey)

	// Initialize ab.
	setupAuthboss()

	// Set up our router
	r := mux.NewRouter()

	// Routes
	gets := r.Methods("GET").Subrouter()
	//posts := r.Methods("POST").Subrouter()

	r.PathPrefix("/auth").Handler(ab.NewRouter())

	gets.Handle("/blogs/new", authProtect(newblog))
	//gets.Handle("/blogs/{id}/edit", authProtect(edit))
	//gets.HandleFunc("/blogs", index)
	//gets.HandleFunc("/", index)

	//posts.Handle("/blogs/{id}/edit", authProtect(update))
	//posts.Handle("/blogs/new", authProtect(create))

	// This should actually be a DELETE but I can't be bothered to make a proper
	// destroy link using javascript atm.
	//gets.Handle("/blogs/{id}/destroy", authProtect(destroy))

	r.NotFoundHandler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNotFound)
		io.WriteString(w, "Not found")
	})

	// Set up our middleware chain
	//http.Handle("/", ab.ExpireMiddleware(r))

	// Start the server
	log.Logger.Println(http.ListenAndServe("localhost:"+"8080", ab.ExpireMiddleware(r)))
}

func layoutData(w http.ResponseWriter, r *http.Request) authboss.HTMLData {
	currentUserName := ""
	userInter, err := ab.CurrentUser(w, r)
	if userInter != nil && err == nil {
		currentUserName = userInter.(*User).Name
	}

	return authboss.HTMLData{
		"loggedin":               userInter != nil,
		"username":               "",
		authboss.FlashSuccessKey: ab.FlashSuccess(w, r),
		authboss.FlashErrorKey:   ab.FlashError(w, r),
		"current_user_name":      currentUserName,
	}
}

//func index(w http.ResponseWriter, r *http.Request) {
//	data := layoutData(w, r).MergeKV("posts", blogs)
//	mustRender(w, r, "index", data)
//}
//
func newblog(w http.ResponseWriter, r *http.Request) {
	data := layoutData(w, r)
	mustRender(w, r, "new", data)
}

func mustRender(w http.ResponseWriter, r *http.Request, name string, data authboss.HTMLData) {
	//data.MergeKV("csrf_token", nosurf.Token(r))
	//err := templates.Render(w, name, data)
	//if err == nil {
	//	return
	//}

	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusInternalServerError)
	//fmt.Fprintln(w, "Error occurred rendering template:", err)
}

func badRequest(w http.ResponseWriter, err error) bool {
	if err == nil {
		return false
	}

	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusBadRequest)
	fmt.Fprintln(w, "Bad request:", err)

	return true
}
