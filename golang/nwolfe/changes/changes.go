package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"nwolfe/utils"
)

var cmd = &cobra.Command{
	Use:   "changes",
	Short: "Show changes in repos",
	RunE: func(cmd *cobra.Command, args []string) error {
		return printChanges()
	},
}

func printChanges() error {
	repos, err := utils.GetRepos()
	if err != nil {
		return err
	}
	for _, repo := range repos {
		status, err := repo.Status(true)
		if err != nil {
			return err
		}
		if len(status) > 0 {
			branch, err := repo.Branch()
			if err != nil {
				return err
			}
			fmt.Printf("%s (%s)\n", repo.Name, branch)
			fmt.Println(status)
		}
	}
	return nil
}

func main() {
	if err := cmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
