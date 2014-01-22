# dotfiles

My configuration files

## Installation

A Rakefile is provided to handle installation. It will symlink most files into your home directory.
It will also run a few `git` commands to fetch the VIM plugin submodules.

```sh
$ git clone git@github.com:nwolfe/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
$ rake install
```

### Mac OS

Configuration is in the [osx](./osx) file. You must source it manually.
