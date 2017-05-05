package db

import (
	"time"

	"fmt"
	"github.com/databrary/databrary/util"
	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq"
	"github.com/spf13/viper"
)

func OpenConn(conf *viper.Viper) (*sqlx.DB, error) {
	settings := fmt.Sprintf("host=%s port=%s user=%s password=%s dbname=%s sslmode=%s",
		conf.GetString("database.host"),
		conf.GetString("database.port"),
		conf.GetString("database.user"),
		conf.GetString("database.password"),
		conf.GetString("database.dbname"),
		conf.GetString("database.sslmode"),
	)
	conn, err := sqlx.Connect("postgres", settings)
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
