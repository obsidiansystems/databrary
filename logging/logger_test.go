package logging

import (
	"testing"

	"encoding/json"
	"github.com/databrary/databrary/config"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
)

func TestInitLogger(t *testing.T) {
	path := "/tmp/test_config.toml"
	file, err := os.Create(path)
	if err != nil {
		log.Fatalln("couldn't create temp config")
	}
	defer file.Close()
	defer os.Remove(path)
	//defer os.Remove(path)
	file.WriteString(`log_path = "/tmp/tmp_log"`)
	file.WriteString("\n")
	file.WriteString(`log_level = "DEBUG"`)
	defer func() {
		tmp_files, _ := filepath.Glob("/tmp/tmp_log*")
		for _, tmp_file := range tmp_files {
			defer os.Remove(tmp_file)
		}
	}()

	config.InitConfig(path)
	c := config.GetConfig()

	InitLogger(c)
	d := GetDbryLgr()
	d.ContextLog.WithFields(MkLgrsFlds(Fields{
		"test1": "test2",
	})).Debug("test")

	byt, err := ioutil.ReadFile("/tmp/tmp_log")
	var dat map[string]interface{}

	if err := json.Unmarshal(byt, &dat); err != nil {
		panic(err)
	}
	if dat["test1"].(string) != "test2" {
		t.Error("log write failed")
	}
}
