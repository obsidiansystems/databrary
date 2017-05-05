package routes

import (
	"net/http"
	"fmt"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/db"
	conf "github.com/databrary/databrary/config"
	"github.com/databrary/databrary/util"
	"encoding/json"
	"github.com/gorilla/mux"
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
