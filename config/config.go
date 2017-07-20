//Viper for config
package config

import (
	"fmt"

	"github.com/spf13/viper"
)

// Use a persistent pointer but don't export in order to implement reasonable
// initialization semantics
var vprConf *viper.Viper

// Initialize config object. Panics on error
func InitConf(config_path string) *viper.Viper {
	if vprConf != nil {
		return vprConf
	}
	viper.SetConfigFile(config_path)
	err := viper.ReadInConfig() // Find and read the config file
	if err != nil {             // Handle errors reading the config file
		panic(fmt.Sprintf("config file error: %s", err))
	} else {
		fmt.Printf("config %s successfully loaded\n", viper.GetViper().ConfigFileUsed())
	}
	vprConf = viper.GetViper()
	return vprConf
}

// Get already initialized config object. Panics if uninitialized
func GetConf() *viper.Viper {
	if vprConf == nil {
		panic("tried to get uninited config")
	}
	return vprConf
}
