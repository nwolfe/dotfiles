# ------------------------------------------------------------------------
# Goodsam oh-my-zsh theme
#
# Inspired by the prompt of Sam Aaron (Emacs Live)
#
# Used herein:
#
# * %c                  Current working directory
#
# * %t                  Clock/timestamp (e.g. 1:09PM)
#
# * parse_git_dirty     Asterisk if dirty, nil otherwise
#
# * git_prompt_ahead    $ZSH_THEME_GIT_PROMPT_AHEAD if master is
#                       ahead of origin/master, nil otherwise
#                       (i.e. unpushed commits to master)
#
# * git_prompt_info     The current branch with parse_git_dirty
#                       decorations when in a git repo, nil otherwise
# ------------------------------------------------------------------------

# Color shortcuts
RED=$fg[red]
YELLOW=$fg[yellow]
GREEN=$fg[green]
WHITE=$fg[white]
BLUE=$fg[blue]
RED_BOLD=$fg_bold[red]
YELLOW_BOLD=$fg_bold[yellow]
GREEN_BOLD=$fg_bold[green]
WHITE_BOLD=$fg_bold[white]
BLUE_BOLD=$fg_bold[blue]
RESET_COLOR=$reset_color

# Git customizations
ZSH_THEME_GIT_PROMPT_AHEAD=" %{$RED%}(!)"
ZSH_THEME_GIT_PROMPT_PREFIX="%{$GREEN_BOLD%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$RESET_COLOR%}"

# Left prompt
PROMPT='
%{$RED_BOLD%}∴%{$RESET_COLOR%} %{$BLUE%}%c$(parse_git_dirty)$(git_prompt_ahead)%{$RESET_COLOR%}
%{$YELLOW_BOLD%}λ%{$RESET_COLOR%} '

# Right prompt
RPROMPT='$(git_prompt_info) %t'
