#!/usr/bin/env bash

# Prints a horizonatal line the full width of the terminal.
# Optional argument $1 for the character to print; defaults to =.
line() {
    char=${1:-=}
    printf '%*s\n' "${COLUMNS:-$(tput cols)}" "${char}" | tr ' ' "${char}"
}
