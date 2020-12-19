#!/usr/bin/env bash

# https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg

# Standard 8 colors
BLACK=$(tput setaf 0)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
MAGENTA=$(tput setaf 5)
CYAN=$(tput setaf 6)
WHITE=$(tput setaf 7)

# Formatting
BOLD=$(tput bold)
RESET=$(tput sgr0)

# Export separately to satisfy shellcheck warnings
export BLACK
export RED
export GREEN
export YELLOW
export BLUE
export MAGENTA
export CYAN
export WHITE
export BOLD
export RESET

_colors() {
    echo "${RED}RED${RESET}"
    echo "${GREEN}GREEN${RESET}"
    echo "${YELLOW}YELLOW${RESET}"
    echo "${BLUE}BLUE${RESET}"
    echo "${MAGENTA}MAGENTA${RESET}"
    echo "${CYAN}CYAN${RESET}"
    echo "${WHITE}WHITE${RESET}"
    echo "${BOLD}BOLD${RESET}"
}
