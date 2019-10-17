package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"nwolfe/utils"
)

func init() {
	var pullCmd = &cobra.Command{
		Use:   "pull",
		Short: "Pull down latest changes for each repository",
		RunE: func(cmd *cobra.Command, args []string) error {
			return pullLatest()
		},
	}
	rootCmd.AddCommand(pullCmd)
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

		targetBranch, ok := config.Pull.Targets[repo.Name]
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
