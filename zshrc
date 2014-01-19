# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="wezm"
#ZSH_THEME="nicoulaj"
#ZSH_THEME="blinks"
#ZSH_THEME="sorin"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# ** Uncommented for use of 'tree' command so it wouldn't try to autocorrect to 'tee'
# ** Recommented for lack of using 'tree'
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git brew)
plugins+=(gem rvm)
plugins+=(lein)

source $ZSH/oh-my-zsh.sh

### MY ADDITIONS ###
source $HOME/.dotfiles/puppetrc

# Source all files in scripts/ if there is any
if [ -d $HOME/scripts ] && [ `ls $HOME/scripts` ]; then
  for script in $HOME/scripts/*
    do source $script
  done
fi

### ENVIRONMENT
VISUAL=vim
TERM=xterm-256color
export PATH=/usr/local/bin:$PATH

### ALIASES
# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Verify destruction
alias rm='rm -iv' # Verbose, print removed files
alias mv='mv -i'
alias cp='cp -i'

# List directory contents
# -h  Human-readable file sizes
# -G  Colored output
# -F  Append special character denoting file type
# -o  Long format, omit owner
# -g  Long format, omit group
alias ls='ls -hGF'
alias ll='ls -og'
alias la='ls -A'

# Shortcuts
alias v='vim'
alias repl='lein trampoline repl'

### FUNCTIONS
# List directory contents after cd
function chpwd() {
  ls
}

# Create executable shell script with hash-bang line
function mkscript() {
  if [ -e $1 ]; then
      echo "ERROR: $1 already exists" 1>&2
      return 1
  fi

  echo "#!/bin/sh" > $1
  chmod 777 $1
  return 0
}

function enable-no-password-login-on() {
  cat ~/.ssh/id_rsa.pub | ssh root@$1 "cat >> ~/.ssh/authorized_keys"
}
