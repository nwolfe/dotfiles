# Use in place of `tee` to write to a logfile under $DOTFILES/.local/logs.

source $DOTFILES/utils/line.sh

log() {
    _logdir=$DOTFILES/.local/logs
    [ ! -d $_logdir ] && mkdir -p $_logdir
    _logfile=$_logdir/$(basename $1)
    # Use "+%Y-%m-%d-%H%M%S" for filename-suitable timestamp
    _timestamp=$(date)
    echo $(line =) > $_logfile
    echo "Timestamp:" $_timestamp >> $_logfile
    echo $(line -) >> $_logfile
    tee -a $_logfile
}
