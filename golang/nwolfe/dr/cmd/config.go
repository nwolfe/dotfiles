package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"gopkg.in/yaml.v3"
)

func init() {
	var configCmd = &cobra.Command{
		Use:   "config",
		Short: "Print configuration file",
		RunE: func(cmd *cobra.Command, args []string) error {
			return printConfig()
		},
	}
	rootCmd.AddCommand(configCmd)
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
