set nocompatible

execute pathogen#infect()

au FileType coq call coquille#CoqideMapping()

colorscheme default

filetype on

filetype plugin indent on

syntax on

set paste
set backupdir=~/.vim/backup
set backspace=indent,eol,start
set wildmode=list:longest
set list
set listchars=tab:>-,trail:-
set number
set ruler
set showmatch
set shiftround
set tabstop=2
set shiftwidth=2
set smarttab
set expandtab
set softtabstop=2
set cindent
set smartindent
set autoindent
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v]
set laststatus=2 " always show the status line
set mouse=a
set colorcolumn=80
set hlsearch

:nnoremap <silent> <F4> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

map <Esc>[7~ <Home>
imap <Esc>[7~ <Home>
map <Esc>[8~ <End>
imap <Esc>[8~ <End>
map <Esc>[5~ <PageUp>
imap <Esc>[5~ <PageUp>
map <Esc>[6~ <PageDown>
imap <Esc>[6~ <PageDown>
map <Esc>[1~ <Home>
imap <Esc>[1~ <Home>
map <Esc>[4~ <End>
imap <Esc>[4~ <End>

if &diff
  colorscheme murphy
endif

au Bufenter *.hs compiler ghc
let g:haddock_browser = "chromium"
