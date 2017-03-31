package config

import (
	"log"
	"os"
	"testing"
)

func TestConfig(t *testing.T) {
	path := "/tmp/test_config.toml"
	file, err := os.Create(path) // For read access.
	if err != nil {
		log.Fatal(err)
	}
	defer os.Remove(path)

	file.WriteString("test = 1\n")
	file.WriteString("test1 = \"2\"\n")

	InitConfig(path)
	config := GetConfig()
	if config.GetIntEntry("test") != 1 {
		t.Error("test != 1")
	}
	// TODO: i don't like casts
	//if config.GetStringEntry("test") == "1" {
	//	t.Error("test != \"1\"")
	//}
	if config.GetStringEntry("test1") != "2" {
		t.Error("test1 != \"2\"")
	}
	//if config.GetIntEntry("test1") == 2 {
	//	t.Error("test1 != 2")
	//}

}
