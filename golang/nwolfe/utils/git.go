package utils

import (
	"os/exec"
	"strings"
)

type Repo struct {
	Path   string
	Name   string
}

func (repo Repo) git(command string, args ...string) (string, error) {
	cmd := exec.Command("git", "-C", repo.Path, command)
	cmd.Args = append(cmd.Args, args...)
	stdoutAndStderr, err := cmd.CombinedOutput()
	return string(stdoutAndStderr[:]), err
}

func (repo Repo) Status(short bool) (string, error) {
	flag := ""
	if short {
		flag = "--short"
	}
	return repo.git("status", flag)
}

func (repo Repo) Branch() (string, error) {
	branch, err := repo.git("rev-parse", "--abbrev-ref", "HEAD")
	return strings.TrimSpace(branch), err
}

func (repo Repo) Checkout(branch string) (string, error) {
	return repo.git("checkout", branch)
}
