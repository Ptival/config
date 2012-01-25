"### Global configuration

set nobackup
filetype plugin indent on
set backspace=indent,eol,start
set wildmode=list:longest
set list
set listchars=tab:>-,trail:-
set number
set ruler
set showmatch
set shiftround
set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
set softtabstop=4
set cindent
set smartindent
set autoindent
set nocompatible
syntax on
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v]
set laststatus=2 " always show the status line
colorscheme slate
set mouse=a
set colorcolumn=80
:nnoremap <silent> <F4> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
set hlsearch

" OMLet specific (OCaml)
let omlet_indent_match = 0
let omlet_indent_function = 0
let omlet_indent_let = 0

au Bufenter *.hs compiler ghc
let g:haddock_browser = "chromium"

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
