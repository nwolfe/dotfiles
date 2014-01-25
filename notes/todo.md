# TO DO:
#   Is this supposed to be a markdown file?

### DOTFILES
[ ] See if there's an already establish or proper way of doing dotfiles/sources in the UNIX world
[ ] Remove hardcoded paths to ~/.dotfiles in various sources
    * Maybe we can define a $DOTFILES variables?

### TMUX
[ ] Add more useful things to the tmux status bar to make it like an OS
[ ] Enable vim bindinds for copy/paste in tmux copy mode
[ ] Add binding that reduces the VIM-delay
[ ] Keymap C-; to clear (now that C-l moves to the right pane)?
    * Maybe I shouldn't be clearing the screen so much?

### VIM
[ ] Install markdown plugin
    [ ] And maybe install vim flavored markdown extension plugin?
[ ] Install ack plugin
[ ] Install vimux plugin

### "NOTES" / 'remember'
[ ] Maybe we don't even want a notes repo?
    * In the end it might just be easier to have it as a part of your dotfiles
[ ] What to do if ~/notes/ doesn't exist?
    * Maybe we can just create it and let them turn it into a git repo if they want?
[ ] Write proper man page for remember?
[ ] Add support for autocompletion?
    [ ] Tab through the available notefiles
[ ] Do something about use of $VISUAL?
    [ ] Warn if it's not set?
    * Maybe there's a better env variable to use here?
