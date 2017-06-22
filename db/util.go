package db

import (
	"time"

	"fmt"
	"github.com/databrary/databrary/util"
	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/spf13/viper"
)

var dbConn *sqlx.DB

func InitDB(conf *viper.Viper) error {
	settings := fmt.Sprintf("host=%s port=%s user=%s password=%s dbname=%s sslmode=%s",
		conf.GetString("database.host"),
		conf.GetString("database.port"),
		conf.GetString("database.user"),
		conf.GetString("database.password"),
		conf.GetString("database.dbname"),
		conf.GetString("database.sslmode"),
	)
	var err error
	if dbConn, err = sqlx.Connect("postgres", settings); err != nil {
		dbConn = nil
		return err
	}
	dbConn.SetConnMaxLifetime(time.Hour)
	dbConn.SetMaxIdleConns(100)
	dbConn.SetMaxOpenConns(100)
	return nil
}

func GetDbConn() (*sqlx.DB, error) {
	if dbConn != nil {
		return dbConn, nil
	}
	return nil, errors.New("uninited db")
}

// pg dates are relative to this fixed zone
func PgDate(t time.Time) time.Time {
	return t.In(time.FixedZone("", 0))
}

func PgToday() time.Time {
	return PgDate(util.Now())
}
