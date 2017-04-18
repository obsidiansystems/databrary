package db

import (
	"time"

	"github.com/databrary/databrary/util"
	"github.com/spf13/viper"
	"upper.io/db.v3/lib/sqlbuilder"
	pg "upper.io/db.v3/postgresql"
)

func OpenConn(conf *viper.Viper) (sqlbuilder.Database, error) {
	settings := &pg.ConnectionURL{
		Host:     conf.GetString("database.addr") + ":" + conf.GetString("database.port"),
		Database: conf.GetString("database.db_name"),
		User:     conf.GetString("database.user"),
		Password: conf.GetString("database.pw"),
	}

	conn, err := pg.Open(settings)
	if err != nil {
		return nil, err
	}

	return conn, nil
}

// pg dates are relative to this fixed zone
func PgDate(t time.Time) time.Time {
	return t.In(time.FixedZone("", 0))
}

func PgToday() time.Time {
	return PgDate(util.Now())
}
