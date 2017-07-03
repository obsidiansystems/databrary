package util

import (
	"math/rand"
	"runtime/debug"
	"strings"
	"time"

	"encoding/json"
	"github.com/databrary/databrary/logging"
	"net/http"
)

var (
	letterRunes = []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
)

func init() {
	rand.Seed(time.Now().UnixNano())
}

func RandStringRunes(n int) string {
	b := make([]rune, n)
	for i := range b {
		b[i] = letterRunes[rand.Intn(len(letterRunes))]
	}
	return string(b)
}

func CheckOrFatalErr(e error) {
	if e != nil {
		// trim the top the 3 lines of the stack because they're inside debug.Stack()
		stack := strings.Split(string(debug.Stack()), "\n")[3:]
		log.Logger.Print(strings.Join(stack, "\n"))
		log.Logger.Fatal(e)
	}
}

func Now() time.Time {
	// postgres rounds
	return time.Now().Round(time.Microsecond)
}

func JsonErrResp(w http.ResponseWriter, code int, data interface{}) {
	w.WriteHeader(code)
	WriteJSONResp(w, "error", data)
}

func WriteJSONResp(w http.ResponseWriter, status string, msg interface{}) error {
	w.WriteHeader(http.StatusOK)
	resp := JSONResponse{status, msg}
	j, _ := json.Marshal(resp)
	_, err := w.Write(j)
	return err
}

type JSONResponse struct {
	Status string      `json:"status"`
	Data   interface{} `json:"data"`
}
