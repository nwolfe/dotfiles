package cmd

import (
	"io/ioutil"
	"path/filepath"

	"github.com/spf13/cobra"
	"gopkg.in/yaml.v3"
	"nwolfe/utils"
)

// Subcommands add themselves to this variable directly
var rootCmd = &cobra.Command{
	Use:   "dr",
	Short: "Manage your $DEV_ROOT repositories.",
}

func Execute() error {
	return rootCmd.Execute()
}

// Command-wide configuration. Global values are at the
// top level, and subcommand-specific configuration is
// under a section named after the subcommand.
type configuration struct {
	Ignored []string `yaml:"ignored"`
	Pull struct {
		Targets map[string]string `yaml:"targets"`
	} `yaml:"pull"`
}

func (config configuration) isIgnored(repo *utils.Repo) bool {
	for _, ignored := range config.Ignored {
		if repo.Name == ignored {
			return true
		}
	}
	return false
}

func configurationFile() (string, error) {
	devroot, err := utils.DevRoot()
	if err != nil {
		return "", err
	}
	return filepath.Join(devroot, "configs", "devroot.yaml"), nil
}

func loadConfiguration() (*configuration, error) {
	configFile, err := configurationFile()
	if err != nil {
		return nil, err
	}

	bytes, err := ioutil.ReadFile(configFile)
	if err != nil {
		return nil, err
	}

	var config configuration
	if err := yaml.Unmarshal(bytes, &config); err != nil {
		return nil, err
	}

	return &config, nil
}
