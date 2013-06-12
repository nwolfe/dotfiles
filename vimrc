" For more information on a setting type :help <setting>

set nocompatible                " Use Vim settings instead of classic vi. Must be first
set encoding=utf-8

set showcmd                     " Display incomplete commands
set showmatch                   " Highlight matching parens
set title                       " Show title
set relativenumber              " Show line numbers relative to the cursor line
set history=500                 " Keep # lines of command line history
set autoread                    " Auto-reload buffers when file changed on disk
set hidden                      " Switch away from a buffer with unsaved changes
set wildmenu                    " Bash-style file & command Tab completion
set wildmode=list:longest
set splitright                  " Split to the right side instead of left
set splitbelow                  " Split below instead of above
set shell=/bin/bash             " For embedded shell to work properly 

"" Color Scheme
colorscheme desert

"" Status Line
set laststatus=2                " Always show status line
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

"" Swap/Backup Files
set noswapfile
set nobackup
set nowritebackup 

"" Cursor
set ruler                       " Show cursor position at bottom
set cursorline                  " Underline the line the cursor is on
set scrolloff=3                 " Begin scrolling the screen # lines from cursor
set sidescrolloff=2

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

syntax enable
filetype plugin on
filetype indent on

" Ruby
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading=1
autocmd FileType ruby,eruby let g:rubycomplete_rails=1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global=1

" Improve Powertab? autocomplete menu color
highlight Pmenu ctermbg=blue gui=bold

"" Mappings
let mapleader=","

" Swap buffers
nnoremap <leader><leader> <C-^>

" Un-highlight search matches on Enter
nnoremap <CR> :nohlsearch<CR>

" Window navigation shortcuts
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Disable arrow keys in normal mode for fun
map <Up> <nop>
map <Down> <nop>
map <Left> <nop>
map <Right> <nop>

" Scroll the viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Edit files from directory of current buffer
" http://vimcasts.org/episodes/the-edit-command
cnoremap %% <C-R>=expand('%:h').'/'<CR>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%
