package routes

import (
	"encoding/json"
	"github.com/databrary/databrary/db"
	"github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/mail"
	"github.com/databrary/databrary/util"
	"github.com/pressly/chi"
	"github.com/renstrom/fuzzysearch/fuzzy"
	"github.com/sirupsen/logrus"
	"github.com/vattle/sqlboiler/queries/qm"
	"gopkg.in/olahol/melody.v1"
	"gopkg.in/throttled/throttled.v2"
	"io/ioutil"
	"net/http"
	"sort"
	"sync"
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
	r.Post("/check-token", CheckTokenExpiryEndpoint)

	r.Post("/register", Register)
}

func AutoCompleteAffil(w http.ResponseWriter, r *http.Request) {
	mrouter := melody.New()
	lock := new(sync.Mutex)
	nInfo := NetInfoLogEntry(r)
	mrouter.HandleConnect(func(s *melody.Session) {

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
		affiliations, err := s.Get("affiliations")
		if err == false {
			log.EntryWrapErr(nInfo, nil, "couldn't get affils")
			return
		}
		matchingAffiliations := fuzzy.RankFindFold(string(affil), affiliations.([]string))
		sort.Sort(matchingAffiliations)
		lenResults := 20
		if matchingAffiliations.Len() < 20 {
			lenResults = matchingAffiliations.Len()
		}
		matchingAffiliations = matchingAffiliations[:lenResults]
		j, _ := json.Marshal(matchingAffiliations)
		mrouter.Broadcast(j)
	})

	mrouter.HandleClose(func(m *melody.Session, i int, s string) error {
		mrouter.Close()
		return nil
	})

	mrouter.HandleError(func(s *melody.Session, err error) {
		mrouter.Close()
	})
	err := mrouter.HandleRequest(w, r)
	if err != nil {
		log.EntryWrapErr(nInfo, err, "couldn't handle websocket request")
	}
}

func ReportError(w http.ResponseWriter, r *http.Request) {
	nInfo := NetInfoLogEntry(r)
	body, err := ioutil.ReadAll(r.Body)
	if err == nil {
		mail.SendEmail(string(body), "Databrary Error", "maksim.levental@nyu.edu")
	} else {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't send report error email")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
	}

}

//func adminRouter() http.Handler {
//	r := chi.NewRouter()
//	r.Use(AdminOnly)
//	r.Get("/", adminIndex)
//	r.Get("/accounts", adminListAccounts)
//	return r
//}
