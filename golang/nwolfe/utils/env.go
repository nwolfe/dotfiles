package utils

import (
	"os"
	"fmt"
)

func DevRoot() (string, error) {
	root := os.Getenv("DEV_ROOT")
	if root == "" {
		return "", fmt.Errorf("DEV_ROOT not defined")
	}
	return root, nil
}
