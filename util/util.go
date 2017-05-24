package util

import (
	"fmt"
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
		logging.Logger.Print(strings.Join(stack, "\n"))
		logging.Logger.Fatal(e)
	}
}

func PrintReps(stuff ...interface{}) {
	for _, v := range stuff {
		fmt.Printf("%#v\n", v)
	}
}

func Now() time.Time {
	// postgres rounds
	return time.Now().Round(time.Microsecond)
}

func Date(t time.Time) time.Time {
	return t.Truncate(24 * time.Hour)
}

func Today() time.Time {
	return Date(Now())
}

func JsonErrorResponse(w http.ResponseWriter, code int, err error, msgf string, args ...interface{}) {
	var msg string
	if len(args) > 0 {
		msg = fmt.Sprintf(msgf, args...)
	} else {
		msg = msgf
	}
	w.WriteHeader(code)
	WriteJSONResp(w, "error", logging.LogWrapAndError(err, msg).Error())
}

func WriteJSONResp(w http.ResponseWriter, status string, msg interface{}) error {
	resp := JSONResponse{status, msg}
	j, _ := json.Marshal(resp)
	_, err := w.Write(j)
	return err
}

type JSONResponse struct {
	Status  string      `json:"status"`
	Payload interface{} `json:"payload"`
}

func MakeMillisecondTimestamp(t time.Time) int64 {
	return t.UnixNano() / (int64(time.Millisecond) / int64(time.Nanosecond))
}
