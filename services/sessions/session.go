package sessions

import "github.com/fatih/structs"

type Session struct {
	LoggedIn bool   `json:"logged_in"`
	ID       string `json:"id"`
	// only for context
	RedirectURI string `json:"redirect_uri"`
	AccountID   int
}

func (s *Session) ToMap() map[interface{}]interface{} {
	stringMap := structs.Map(s)
	// omit RedirectURI because that's only per request context
	m := map[interface{}]interface{}{}
	m["LoggedIn"] = stringMap["LoggedIn"]
	m["ID"] = stringMap["ID"]
	m["AccountID"] = stringMap["AccountID"]
	return m
}

func FromMap(m map[interface{}]interface{}) Session {
	// omit RedirectURI because that's only per request
	return Session{
		LoggedIn:  m["LoggedIn"].(bool),
		ID:        m["ID"].(string),
		AccountID: m["AccountID"].(int),
	}
}
