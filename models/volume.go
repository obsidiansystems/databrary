package models

import (
	//"time"
	"database/sql"
	"database/sql/driver"
	"fmt"
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
		Individual Permission    `json:"volume_access_individual" db:"individual"`
		Children   Permission    `json:"volume_access_children" db:"permission"`
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
type ReleaseLevel string

// Levels at which participants or researchers may choose to share data.
const (
	ReleaseLevelNONE     ReleaseLevel = ReleaseLevel("NONE")
	ReleaseLevelEXCERPTS ReleaseLevel = ReleaseLevel("EXCERPTS")
	ReleaseLevelPUBLIC   ReleaseLevel = ReleaseLevel("PUBLIC")
	ReleaseLevelPRIVATE  ReleaseLevel = ReleaseLevel("PRIVATE")
	ReleaseLevelSHARED   ReleaseLevel = ReleaseLevel("SHARED")
)

func (exp ReleaseLevel) Value() (driver.Value, error) {
	// value needs to be a base driver.Value type
	// such as bool.
	return string(exp), nil
}

func (exp *ReleaseLevel) Scan(value interface{}) error {
	if value == nil {
		return ReleaseLevelErrorDatabase
	}
	if exposure_val, err := driver.String.ConvertValue(value); err == nil {
		if v, ok := exposure_val.(ReleaseLevel); ok {
			*exp = ReleaseLevel(v)
			return nil
		}
	}
	return ReleaseLevelErrorScan
}

// house keeping

// checking to make sure interface is implemented
var _ sql.Scanner = (*ReleaseLevel)(nil)
var _ driver.Valuer = ReleaseLevelNONE

type ReleaseLevelError struct {
	message string
}

func (e *ReleaseLevelError) Error() string {
	return fmt.Sprintf("%s", e.message)
}

var ReleaseLevelErrorDatabase = &ReleaseLevelError{"got nil value from database for ReleaseLevel for volume"}
var ReleaseLevelErrorScan = &ReleaseLevelError{"failed to scan ReleaseLevel"}