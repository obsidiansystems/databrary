// main.go
package main

import (
	"net/http"

	//"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/databrary/databrary/config"
	log "github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/routes"
	"github.com/databrary/databrary/services/sessions"
	"github.com/pressly/chi"
	"github.com/pressly/chi/middleware"
	"github.com/unrolled/secure" // or "gopkg.in/unrolled/secure.v1"
	"gopkg.in/alecthomas/kingpin.v2"
)

var (
	config_path = kingpin.Flag("config", "Path to config file").
		Default(filepath.Join(os.Getenv("GOPATH"), "src/github.com/databrary/databrary/config/databrary_dev.toml")).
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
	sessions.InitStore(config.GetConf())

}

func main() {
	// New permissions middleware
	//conf := config.GetConf()
	secureMiddleware := secure.New(secure.Options{
		AllowedHosts:          []string{"localhost:3000", "localhost:3444"},
		HostsProxyHeaders:     []string{"X-Forwarded-Host"},
		SSLRedirect:           true,
		SSLHost:               "localhost:3000",
		SSLProxyHeaders:       map[string]string{"X-Forwarded-Proto": "https"},
		STSSeconds:            315360000,
		STSIncludeSubdomains:  true,
		STSPreload:            true,
		FrameDeny:             true,
		ContentTypeNosniff:    true,
		BrowserXssFilter:      true,
		ContentSecurityPolicy: "default-src 'self'",
		PublicKey:             `pin-sha256="base64+primary=="; pin-sha256="base64+backup=="; max-age=5184000; includeSubdomains; report-uri="https://www.example.com/hpkp-report"`,
		IsDevelopment:         true,
	})
	_ = secureMiddleware
	r := chi.NewRouter()
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(log.NewStructuredLogger(log.Logger))
	r.Use(middleware.Recoverer)
	//r.Use(secureMiddleware.Handler) // TODO turn back on
	r.Use(middleware.Timeout(60 * time.Second))
	r.Use(routes.Session)
	r.Mount("/api", routes.Api())
	r.With(routes.IsLoggedIn).Get("/profile", routes.GetProfile)
	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("nothing here yet"))
	})

	if err := http.ListenAndServe(":3444", r); err != nil {
		log.LogAndErrorf("couldn't serve: %+v", err)
	}

	// TODO use ssl
	//go http.ListenAndServe(":3444", secureMiddleware.Handler(myHandler))
	//GOPATH := os.Getenv("GOPATH")
	//certPath := filepath.Join(GOPATH, conf.GetString("ssl.cert"))
	//keyPath := filepath.Join(GOPATH, conf.GetString("ssl.key"))
	//err := http.ListenAndServeTLS(":3000", certPath, keyPath, r)
	//fmt.Println(err)
}
