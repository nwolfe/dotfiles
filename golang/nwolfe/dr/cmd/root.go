package cmd

import "github.com/spf13/cobra"

// Subcommands add themselves to this variable directly
var rootCmd = &cobra.Command{
	Use:   "dr",
	Short: "Manage your $DEV_ROOT repositories.",
}

func Execute() error {
	return rootCmd.Execute()
}
