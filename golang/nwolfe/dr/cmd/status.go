package cmd

import (
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/spf13/cobra"
	"nwolfe/utils"
)

func init() {
	var statusCmd = &cobra.Command{
		Use:   "status",
		Short: "Status overview of each repository",
		RunE: func(cmd *cobra.Command, args []string) error {
			return printStatus()
		},
	}
	rootCmd.AddCommand(statusCmd)
}

func printStatus() error {
	config, err := loadConfiguration()
	if err != nil {
		return err
	}

	repos, err := utils.GetRepos()
	if err != nil {
		return err
	}

	w := tabwriter.NewWriter(os.Stdout, 4, 4, 4, ' ', 0)
	fmt.Fprintln(w, "REPOSITORY\tBRANCH\tSTATUS\tHEAD")

	for _, repo := range repos {
		if config.isIgnored(&repo) {
			continue
		}
		repository := repo.Name
		branch := getBranch(&repo)
		diff := getDiff(&repo)
		head := getHead(&repo)
		fmt.Fprintf(w, "%s\t%s\t%s\t%s\n",
			repository, branch, diff, head)
	}

	return w.Flush()
}

func getBranch(repo *utils.Repo) interface{} {
	branch, err := repo.Branch()
	if err != nil {
		return err
	}
	return branch
}

func getDiff(repo *utils.Repo) interface{} {
	diff, err := repo.DiffShort()
	if err != nil {
		return err
	}
	return diff
}

func getHead(repo *utils.Repo) interface{} {
	head, err := repo.HeadOverview()
	if err != nil {
		return err
	}
	return head
}
