package config


import (
	//"github.com/databrary/databrary/logging"
	"fmt"
	"time"

	"github.com/spf13/viper"
)


type Config interface {
	setConfigFile(path string)
	GetEntry(key string) interface{}
	GetBoolEntry(key string) bool
	GetFloat64Entry(key string) float64
	GetIntEntry(key string) int
	GetStringEntry(key string) string
	GetStringMapEntry(key string) map[string]interface{}
	GetStringMapStringEntry(key string) map[string]string
	GetStringSliceEntry(key string) []string
	GetTimeEntry(key string) time.Time
	GetDurationEntry(key string) time.Duration
}


type viperConfig struct {
	vp *viper.Viper
}


func (v *viperConfig) setConfigFile(path string) {
	v.vp.SetConfigFile(path)
}


func (v *viperConfig) GetEntry(key string) interface{} {
	if v.vp.IsSet(key) {
		return v.vp.Get(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetBoolEntry(key string) bool {
	if v.vp.IsSet(key) {
		return v.vp.GetBool(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetFloat64Entry(key string) float64 {
	if v.vp.IsSet(key) {
		return v.vp.GetFloat64(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetIntEntry(key string) int {
	if v.vp.IsSet(key) {
		return v.vp.GetInt(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetStringEntry(key string) string {
	if v.vp.IsSet(key) {
		return v.vp.GetString(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetStringMapEntry(key string) map[string]interface{} {
	if v.vp.IsSet(key) {
		return v.vp.GetStringMap(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetStringMapStringEntry(key string) map[string]string {
	if v.vp.IsSet(key) {
		return v.vp.GetStringMapString(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetStringSliceEntry(key string) []string {
	if v.vp.IsSet(key) {
		return v.vp.GetStringSlice(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetTimeEntry(key string) time.Time {
	if v.vp.IsSet(key) {
		return v.vp.GetTime(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


func (v *viperConfig) GetDurationEntry(key string) time.Duration {
	if v.vp.IsSet(key) {
		return v.vp.GetDuration(key)
	} else {
		panic(fmt.Sprintf("key %s missing from config", key))
	}
}


var config *viperConfig = nil


// Initialize config
//
// path: complete and absolute path to config file
func InitConfig(path string) {
	if config != nil {
		return
	}

	config = &viperConfig{vp: viper.New()}
	config.setConfigFile(path)
	err := config.vp.ReadInConfig() // Find and read the config file
	if err != nil { // Handle errors reading the config file
		panic(fmt.Sprintf("config file error: %s", err))
	} else {
		fmt.Printf("config %s successfully loaded\n", viper.GetViper().ConfigFileUsed())
	}
}


func GetConfig() *viperConfig {
	if config == nil {
		panic("config wasn't initialized properly")
	}
	return config
}

// book keeping
// check that viperConfig implements Config
var _ Config = (*viperConfig)(nil)