package main

import (
	"gopkg.in/yaml.v3"
)

// go:embed config.yaml
var CONFIG_FILE []byte // the location of the config file

type Config struct {
	Go              []string `yaml:"go"`
	Comm_par_values []int    `yaml:"comm_par_values"`
}

func parseConfigFile() Config {
	var config Config

	yaml.Unmarshal(CONFIG_FILE, &config)

	return config
}
