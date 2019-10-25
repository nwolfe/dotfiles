// Package cmd contains commands available on the dr CLI.
//
// Ideas for new functionality below.
//
// * Groups of Repos
//
// Allow a group of repos to be defined and operated on, rather
// than operate on all repos found in configuration file.
//
// For example, we could have a "FOSS" and "PE" group, where
// the FOSS group would have all the FOSS-only repos, and the
// PE group would have all the PE-only repos.
//
// Then a group could be optionally provided to a subcommand
// to restrict what repos it operates on, such as `dr status pe`
// or `dr pull foss`.
//
// Maybe the groups are based on teams? For instance, a 'pupperware'
// group that contains all the repos you care about when doing work
// for the pupperware team. Similarly, a 'cd4pe' group that contains
// all the repos you care about when doing work for the cd4pe team.
//
// * Ticket Branches
//
// Branch management across repos based on a common JIRA/ticket number.
//
// Associate repos and branches with a common JIRA/ticket/change, and
// then switch between the common change and have all repos and branches
// managed accordingly.
//
// For instance, you might have a "maint/logdir" changeset and a
// "jra123/feature" changeset, and each one has specific repos and branches
// associated with them. Then you could switch between working on either of
// them and have the branches of each associated repo be checked out.
//
// This would help with managing unrelated changes to the same repos.
//
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

// Execute the CLI, hiding the implemetation from main.go
func Execute() error {
	return rootCmd.Execute()
}

// Command-wide configuration. Global values are at the
// top level, and subcommand-specific configuration is
// under a section named after the subcommand.
type configuration struct {
	Ignored []string `yaml:"ignored"`
	Pull    struct {
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
