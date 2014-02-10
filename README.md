# dotfiles

My configuration files

## Contents

* `Rakefile`: Installer; run `rake install`
* `lib/`: Dotfiles to be symlinked into home directory
* `bin/`: Executables; directory symlinked into home directory and added to $PATH
* `sources/`: Files sourced upon startup (e.g. aliases, functions, exports)
* `vim/`: VIM home; symlinked into home directory
* `live-packs/`: Emacs Live customization home; symlinked into home directory
* `completion/`: ZSH command completion files
* `misc/`: Better defaults for Mac OS, oh-my-zsh theme, etc.

## Installation

A Rakefile is provided to handle installation. It will symlink most files into your home directory.
It will also run a few `git` commands to fetch the VIM plugin submodules.

```sh
$ git clone git@github.com:nwolfe/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
$ rake install
```

#### Mac OS

Configuration is in the [osx](./misc/osx) file. You must source it manually.
