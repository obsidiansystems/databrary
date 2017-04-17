package util

import (
	"fmt"
	log "github.com/databrary/databrary/logging"
	"math/rand"
	"runtime/debug"
	"strings"
	"time"
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

func CheckErr(e error) {
	if e != nil {
		// trim the top the 3 lines of the stack because they're inside debug.Stack()
		stack := strings.Split(string(debug.Stack()), "\n")[3:]
		log.Logger.Print(strings.Join(stack, "\n"))
		log.Logger.Fatal(e)
	}
}

func PrintReps(stuff ...interface{}) {
	for _, v := range stuff {
		fmt.Printf("%#v\n", v)
	}
}
