set nocompatible                " Use Vim settings instead of classic vi. Must be first
set encoding=utf-8

syntax enable                   " Syntax highlighting
filetype plugin indent on       " Load file type plugins + indentation
color desert                    " Set the color theme

set showcmd                     " Display incomplete commands
set showmatch                   " Highlight matching parens
set ruler                       " Show cursor position at bottom
set title                       " Show title
set cursorline                  " Underline the line the cursor is on
set number                      " Show line numbers
set history=500                 " Keep # lines of command line history
set autoread                    " Auto-reload buffers when file changed on disk
set wildmenu                    " Bash-style file & command Tab completion
set wildmode=list:longest

" Buffers can exist in background without being in a window
set hidden

" Highlight current status line when multiple windows are open
highlight StatusLine ctermfg=blue ctermbg=yellow

"" Swap Files
set updatecount=0               " Disable swap files

"" Whitespace
set tabstop=2 shiftwidth=2      " Tab is 2 spaces
set expandtab                   " Use spaces instead of tabs
set backspace=indent,eol,start  " Backspace through everything in insert mode
set nowrap                      " Don't wrap lines by default
set linebreak                   " Break on words when lines are wrapped
set autoindent

"" Searching
set hlsearch                    " Highlight matches
set incsearch                   " Search as you type
set ignorecase                  " Searches ignore case...
set smartcase                   " ...unless they contain at least one capital letter

"" Mappings
" Un-highlight search matches on Enter
nnoremap <CR> :nohlsearch<CR>

" Window navigation shortcuts
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Disable arrow keys in normal mode for fun
map <Up> :echo "no!"<CR>
map <Down> :echo "no!"<CR>
map <Left> :echo "no!"<CR>
map <Right> :echo "no!"<CR>
