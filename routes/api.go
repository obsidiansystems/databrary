package routes

import (
	"github.com/Sirupsen/logrus"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/mail"
	"github.com/pressly/chi"
	"gopkg.in/throttled/throttled.v2"
	"github.com/databrary/databrary/db"
	"io/ioutil"
	"net/http"
	"gopkg.in/olahol/melody.v1"
	"sync"
	"github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/vattle/sqlboiler/queries/qm"
	"github.com/renstrom/fuzzysearch/fuzzy"
	"encoding/json"
	"fmt"
	//"github.com/pressly/chi/middleware"
)

var rateLimiter throttled.HTTPRateLimiter

func init() {
	var err error
	rateLimiter, err = NewRateLimiter()
	if err != nil {
		log.WrapErrLogFatal(err, "couldn't create rate limiter")
	}
}

// A completely separate router for administrator routes
func Api() http.Handler {
	r := chi.NewRouter()
	r.With(rateLimiter.RateLimit).Route("/user", user)
	r.With(rateLimiter.RateLimit).Get("/loggedin", IsLoggedInEndpoint)
	r.With(rateLimiter.RateLimit).Post("/report-error", ReportError)
	r.Get("/autocomplete-affil", AutoCompleteAffil)
	return r
}

func NetInfoLogEntry(r *http.Request) *logrus.Entry {
	fields := logrus.Fields(map[string]interface{}{
		"uri": r.RequestURI,
		"ip":  r.RemoteAddr,
	})
	return log.Logger.WithFields(fields)
}

func user(r chi.Router) {

	r.Post("/login", PostLogin)
	r.Post("/logout", PostLogOut)
	r.Get("/login", GetLogin) //TODO remove

	r.Post("/reset-password/email", ResetPasswordEmail)
	r.Post("/reset-password/token", ResetPasswordToken)

	r.Get("/exists", UserExists)
}

func AutoCompleteAffil(w http.ResponseWriter, r *http.Request) {
	mrouter := melody.New()
	lock := new(sync.Mutex)
	nInfo := NetInfoLogEntry(r)
	mrouter.HandleConnect(func(s *melody.Session) {

		fmt.Println("1Adfadfadfdf")
		dbConn, err := db.GetDbConn()

		if err != nil {
			log.EntryWrapErr(nInfo, err, "couldn't open db conn")
			return
		}

		affilsPartySlice, err := public.Parties(dbConn, qm.Select("distinct affiliation"), qm.Where("affiliation IS NOT NULL")).All()
		if err != nil {
			log.EntryWrapErr(nInfo, err, "couldn't get affils")
			return
		}

		affilsString := make([]string, len(affilsPartySlice))
		for _, affil := range affilsPartySlice {
			affilsString = append(affilsString, affil.Affiliation.String)
		}
		lock.Lock()
		s.Set("affiliations", affilsString)
		lock.Unlock()

	})

	mrouter.HandleMessage(func(s *melody.Session, affil []byte) {
		fmt.Println("2Adfadfadfdf")
		affiliations := s.MustGet("affiliations")
		matchingAffiliations := fuzzy.Find(string(affil), affiliations.([]string))
		j, _ := json.Marshal(matchingAffiliations)
		mrouter.Broadcast(j)
	})

	mrouter.HandleSentMessage(func(s *melody.Session, b []byte) {
		fmt.Println(string(b))
	})

	mrouter.HandleError(func(s *melody.Session, err error) {
		fmt.Println("3Adfadfadfdf")
		_ = s
		fmt.Println(err.Error())
	})
	err := mrouter.HandleRequest(w, r)
	fmt.Println(err)
}

func ReportError(_ http.ResponseWriter, r *http.Request) {
	body, err := ioutil.ReadAll(r.Body)
	if err == nil {
		mail.SendEmail(string(body), "Databrary Error", "maksim.levental@nyu.edu")
	}
}

//func adminRouter() http.Handler {
//	r := chi.NewRouter()
//	r.Use(AdminOnly)
//	r.Get("/", adminIndex)
//	r.Get("/accounts", adminListAccounts)
//	return r
//}
