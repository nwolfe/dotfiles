package utils

import (
	"path/filepath"
)

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
