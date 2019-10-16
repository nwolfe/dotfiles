package utils

import (
	"os/exec"
	"strings"
)

type Repo struct {
	Path   string
	Name   string
}

func (repo Repo) Git(command string, args ...string) (string, error) {
	cmd := exec.Command("git", "-C", repo.Path, command)
	cmd.Args = append(cmd.Args, args...)
	stdoutAndStderr, err := cmd.CombinedOutput()
	return string(stdoutAndStderr[:]), err
}

func (repo Repo) StatusShort() (string, error) {
	return repo.Git("status", "--short")
}

func (repo Repo) Branch() (string, error) {
	result, err := repo.Git("rev-parse", "--abbrev-ref", "HEAD")
	return strings.TrimSpace(result), err
}

func (repo Repo) DiffShort() (string, error) {
	result, err := repo.Git("diff", "--shortstat")
	return strings.TrimSpace(result), err
}

func (repo Repo) HeadOverview() (string, error) {
	return repo.Git(
		"log", "-1", "--color", "--abbrev=7", "--date=relative",
		"--pretty=format:%C(bold white)%h%Creset %s %C(green)(%cr)%Creset")
}
