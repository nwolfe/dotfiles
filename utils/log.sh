#!/usr/bin/env bash

# Use in place of `tee` to write to a logfile under $DOTFILES/.local/logs.
#
# Usage in a script called foo.sh:
#
#   source "$DOTFILES"/utils/log.sh
#   echo foo | log "$0"
#
# Will tee the output to stdout and to the $DOTFILES/.local/logs/foo.sh file.

source "$DOTFILES/utils/line.sh"

log() {
    _logdir=$DOTFILES/.local/logs
    [ ! -d "$_logdir" ] && mkdir -p "$_logdir"
    _logfile=$_logdir/$(basename "$1")
    # Use "+%Y-%m-%d-%H%M%S" for filename-suitable timestamp
    _timestamp=$(date)
    line = > "$_logfile"
    echo "Timestamp:" "$_timestamp" >> "$_logfile"
    line - >> "$_logfile"
    tee -a "$_logfile"
}
