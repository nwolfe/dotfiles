package main

import (
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/spf13/cobra"
	"nwolfe/utils"
)

var cmd = &cobra.Command{
	Use:   "branches",
	Short: "Show branch each repo is on",
	RunE: func(cmd *cobra.Command, args []string) error {
		return printBranches()
	},
}

func printBranches() error {
	repos, err := utils.GetRepos()
	if err != nil {
		return err
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 0, 1, ' ', 0)

	for _, repo := range repos {
		branch, err := repo.Branch()
		if err != nil {
			return err
		}
		fmt.Fprintf(w, "%s\t%s\n", repo.Name, branch)
	}

	if err := w.Flush(); err != nil {
		return err
	}

	return nil
}

func main() {
	if err := cmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
