package routes

import (
	"encoding/json"
	"fmt"
	conf "github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/util"
	"github.com/gorilla/mux"
	"net/http"
	"strconv"
)

func ViewUser(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id, _ := strconv.Atoi(vars["id"])
	conn, err := db.OpenConn(conf.GetConf())
	util.CheckOrFatalErr(err)
	fmt.Fprintln(w, "Welcome!")
	p, err := public_models.FindParty(conn, id)
	j, err := json.Marshal(p)
	fmt.Println(p)
	fmt.Fprint(w, string(j))
}
