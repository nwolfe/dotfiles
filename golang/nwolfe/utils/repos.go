package utils

import (
	"os/exec"
	"path/filepath"
	"strings"
)

type Repo struct {
	Path   string
	Name   string
}

func (repo Repo) Status(short bool) (string, error) {
	flag := ""
	if short {
		flag = "--short"
	}
	cmd := exec.Command("git", "-C", repo.Path, "status", flag)
	status, err := cmd.Output()
	if err != nil {
		return "", err
	}
	return string(status[:]), nil
}

func (repo Repo) Branch() (string, error) {
	cmd := exec.Command("git", "-C", repo.Path, "rev-parse", "--abbrev-ref", "HEAD")
	branch, err := cmd.Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(branch[:])), nil
}

// Gets all git repository directories immediately under $DEV_ROOT.
func GetRepos() ([]Repo, error) {
	root, err := DevRoot()
	if err != nil {
		return nil, err
	}

	gitdirs, err := filepath.Glob(filepath.Join(root, "*", ".git"))
	if err != nil {
		return nil, err
	}

	repos := []Repo{}
	for _, gitdir := range gitdirs {
		path := filepath.Dir(gitdir)
		repo := Repo{
			Path: path,
			Name: filepath.Base(path),
		}
		repos = append(repos, repo)
	}

	return repos, nil
}
