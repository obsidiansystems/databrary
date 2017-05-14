package routes

import (
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	"github.com/jmoiron/sqlx"
	"net/http"
)

func getDb(r *http.Request) (*sqlx.DB, error) {
	var err error
	conn, ok := r.Context().Value("db_conn").(*sqlx.DB)
	if conn == nil || !ok {
		conn, err = db.OpenConn(config.GetConf())
		if err != nil {
			return nil, err
		}
	}
	return conn, nil
}
