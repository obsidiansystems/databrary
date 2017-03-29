package models

import (
	//"time"
	"database/sql"
	"database/sql/driver"
	"fmt"
	"reflect"
)

type (
	// Basic organizational unit for data.
	Volume struct {
		VolumeID uint           `json:"volume_id" db:"id,omitempty"`
		Name     string         `json:"volume_name" db:"name"`
		Body     sql.NullString `json:"volume_body" db:"body"`
		Alias    sql.NullString `json:"volume_alias" db:"alias"` // Short, internal, code name for this volume, for contributors to reference their own data.
		DOI      sql.NullString `json:"volume_doi" db:"doi"`     // DOIs issued for volumes (currently via EZID).
	}

	// Permissions over volumes assigned to users.
	VolumeAccess struct {
		Volume     uint          `json:"volume_access_volume" db:"volume"` // references volume.id
		Party      uint          `json:"volume_access_party" db:"party"`
		Individual Perm          `json:"volume_access_individual" db:"individual"`
		Children   Perm          `json:"volume_access_children" db:"permission"`
		Sort       sql.NullInt64 `json:"volume_access_sort" db:"sort"`
	}

	// Materialized version of volume_owners_view.
	VolumeOwners struct {
		Volume uint     `json:"volume_owners_volume" db:"volume"` // references volume.id
		Owners []string `json:"volume_owners_owners" db:"owners"`
	}

	// Links from volumes to externals resources.
	VolumeLink struct {
		Volume uint   `json:"volume_link_volume" db:"volume"` // references volume.id
		Head   string `json:"volume_link_head" db:"head"`
		Url    string `json:"volume_link_url" db:"url"`
	}

	// Publications/products corresponding to study volumes.
	VolumeCitation struct {
		Volume uint           `json:"volume_citation_volume" db:"volume"` // references volume.id
		Head   string         `json:"volume_citation_head" db:"head"`
		Url    sql.NullString `json:"volume_citation_url" db:"url"`
		Year   uint           `json:"volume_citation_year" db:"year"`
	}

	// Sources of funding, basically a mirror of fundref data, with local party associations (primarily for transition).
	Funder struct {
		FunderReferenceID uint   `json:"funder_reference_id" db:"fundref_id,omitempty"` // Identifiers from fundref.org, under the http://dx.doi.org/10.13039/ namespace. Specifications suggest these may not be numeric, but they seem to be.
		Name              string `json:"funder_name" db:"name"`
	}

	// Funding sources associated with a volume, based on fundref.org.
	VolumeFunding struct {
		Volume uint     `json:"volume_funding_volume" db:"volume"` // references volume.id
		Funder uint     `json:"volume_funding_funder" db:"funder"` // references funder.id
		Awards []string `json:"volume_funding_awards" db:"awards"` // Individual grant identifiers associated with this funder.
	}
)

// ReleaseLevel enum
type ReleaseLvl string

// Levels at which participants or researchers may choose to share data.
const (
	ReleaseLvlNONE     ReleaseLvl = ReleaseLvl("NONE")
	ReleaseLvlEXCERPTS ReleaseLvl = ReleaseLvl("EXCERPTS")
	ReleaseLvlPUBLIC   ReleaseLvl = ReleaseLvl("PUBLIC")
	ReleaseLvlPRIVATE  ReleaseLvl = ReleaseLvl("PRIVATE")
	ReleaseLvlSHARED   ReleaseLvl = ReleaseLvl("SHARED")
)

func (rls ReleaseLvl) Value() (driver.Value, error) {
	// value needs to be a base driver.Value type
	// such as bool.
	return string(rls), nil
}

func (rls *ReleaseLvl) Scan(value interface{}) error {
	if value == nil {
		return RlsLvlErrDb{
			msg: "got nil value from database for ReleaseLevel for volume",
		}
	}
	if exposure_val, err := driver.String.ConvertValue(value); err == nil {
		if v, ok := exposure_val.(ReleaseLvl); ok {
			*rls = ReleaseLvl(v)
			return nil
		}
	}
	return RlsLvlErrScn{
		msg: "failed to scan ReleaseLevel",
	}
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*ReleaseLvl)(nil)
var _ driver.Valuer = ReleaseLvlNONE

type RlsLvlErr interface {
	error
}

type RlsLvlErrDb struct {
	msg string
}

func (e RlsLvlErrDb) Error() string {
	return fmt.Sprintf("%s", e.msg)
}

type RlsLvlErrScn struct {
	msg string
}

func (e RlsLvlErrScn) Error() string {
	return fmt.Sprintf("%s", e.msg)
}

func Test_error() error {
	var test RlsLvlErr = RlsLvlErrScn{
		msg: "test string",
	}
	fmt.Printf("'%#v'", &test)
	fmt.Println(reflect.TypeOf(test))
	err, ok := test.(RlsLvlErrScn)
	fmt.Println(err, ok)
	return err
}
