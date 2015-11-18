# dotfiles

My configuration files

## Contents

* `install`: Installer; run `./install`
* `lib/`: Dotfiles to be symlinked into home directory
* `bin/`: Executables; directory symlinked into home directory and added to $PATH
* `sources/`: Files sourced upon startup (e.g. aliases, functions, exports)
* `vim/`: VIM home; symlinked into home directory
* `completion/`: ZSH command completion files; see also `sources/completions`
* `misc/`: Better defaults for Mac OS, oh-my-zsh theme, etc.

## Installation

An install script is provided to handle installation. It will symlink most
files into your home directory.

```sh
$ git clone git@github.com:nwolfe/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
$ ./install
```

> IMPORTANT: Install script must be run from the dotfiles/ directory

#### Mac OS

Configuration is in the [osx](./misc/osx) file. You must source it manually.
