package custom_types

import (
	"database/sql"
	"database/sql/driver"

	"github.com/databrary/databrary/logging"
)

// ReleaseLevel enum
type ReleaseLvl sql.NullString

// Levels at which participants or researchers may choose to share data.
var (
	ReleaseLvlEXCERPTS ReleaseLvl = ReleaseLvl{"EXCERPTS", true}
	ReleaseLvlPUBLIC   ReleaseLvl = ReleaseLvl{"PUBLIC", true}
	ReleaseLvlPRIVATE  ReleaseLvl = ReleaseLvl{"PRIVATE", true}
	ReleaseLvlSHARED   ReleaseLvl = ReleaseLvl{"SHARED", true}
)

func (rls ReleaseLvl) Value() (driver.Value, error) {
	// value needs to be a base driver.Value type
	// such as bool.
	if !rls.Valid {
		return nil, nil
	}
	return rls.String, nil
}

func (rls *ReleaseLvl) Scan(value interface{}) error {
	if value == nil {
		rls.String, rls.Valid = "", false
		return nil
	}
	if exposure_val, err := driver.String.ConvertValue(value); err == nil {
		if v, ok := exposure_val.([]byte); ok {
			*rls = ReleaseLvl{string(v), true}

			return nil
		}
	}
	return logging.LogAndError("failed to scan ReleaseLevel")
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*ReleaseLvl)(nil)
var _ driver.Valuer = ReleaseLvlSHARED
