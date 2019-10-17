package cmd

import (
	"fmt"
	"io/ioutil"
	"path/filepath"

	"github.com/spf13/cobra"
	"gopkg.in/yaml.v3"
	"nwolfe/utils"
)

func init() {
	var latestCmd = &cobra.Command{
		Use:   "latest",
		Short: "Pull down latest changes for each repository",
		RunE: func(cmd *cobra.Command, args []string) error {
			return pullLatest()
		},
	}
	rootCmd.AddCommand(latestCmd)

	var configCmd = &cobra.Command{
		Use:   "config",
		Short: "Print configuration file",
		RunE: func(cmd *cobra.Command, args []string) error {
			return printConfig()
		},
	}
	latestCmd.AddCommand(configCmd)
}

type configuration struct {
	Ignored []string          `yaml:"ignored"`
	Targets map[string]string `yaml:"targets"`
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
	return filepath.Join(devroot, "configs", "devroot", "latest.yaml"), nil
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

func pullLatest() error {
	config, err := loadConfiguration()
	if err != nil {
		return err
	}

	repos, err := utils.GetRepos()
	if err != nil {
		return err
	}

	for _, repo := range repos {
		if config.isIgnored(&repo) {
			continue
		}

		targetBranch, ok := config.Targets[repo.Name]
		if ok {
			fmt.Printf("%s => %s\n", repo.Name, targetBranch)
			out, err := updateRepo(&repo, targetBranch)
			fmt.Println(out)
			if err != nil {
				fmt.Println(err)
				fmt.Println("")
			}
		}
	}

	return nil
}

func updateRepo(repo *utils.Repo, targetBranch string) (string, error) {
	currentBranch, err := repo.Branch()
	if err != nil {
		return "", err
	}

	if currentBranch != targetBranch {
		out, err := repo.Checkout(targetBranch)
		if err != nil {
			return out, err
		}
	}

	out, err := repo.Pull()
	if err != nil {
		return out, err
	}

	// out, err = repo.BranchDelete(currentBranch)
	// if err != nil {
	// 	return out, err
	// }

	return out, nil
}

func printConfig() error {
	config, err := loadConfiguration()
	if err != nil {
		return err
	}
	bytes, err := yaml.Marshal(config)
	if err != nil {
		return err
	}
	fmt.Println(string(bytes))
	return nil
}
