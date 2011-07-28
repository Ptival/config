"### Global configuration

filetype plugin indent on
set backspace=indent,eol,start
set wildmode=list:longest
"set cursorcolumn
set cursorline
set list
set listchars=tab:>-,trail:-
set number
set ruler
set showmatch
set shiftround
" indent width
set tabstop=4
" shift width for using < or >
set shiftwidth=4
" Use shiftwidth for <TAB> instead of tabstop (useful when tabstop is 8)
set smarttab
" Spaces for indenting
set expandtab
" Deletes multiple spaces with Backspace (for tabulations)
set softtabstop=4
" Automatically indents newline like last line
set cindent
set smartindent
set autoindent
set nocompatible
syntax on
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v]
set laststatus=2 " always show the status line
colorscheme delek
set guifont=Monospace\ 8
set mouse=a

"### Python specific
" Add a tab after newline for special keywords
"autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
" Automatically trim trailing whitespaces on save
"autocmd BufWritePre *.py normal m`:%s/\s\+$//e``

"### Customized shortcuts
" F2 toggles pastemode (no tabs on newlines)
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
" F4 removes trailing whitespaces
:nnoremap <silent> <F4> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" F5 removes ^M
map <F5> :%s/\r//g<CR>

"### HASKELL
" use ghc functionality for haskell files
au Bufenter *.{hs,lhs} compiler ghc
" switch on syntax highlighting
syntax on
" enable filetype detection, plus loading of filetype plugins
filetype plugin on
" configure browser for haskell_doc.vim
let g:haddock_browser = "/usr/bin/google-chrome"

" Settings for VimClojure
let vimclojure#HighlightBuiltins = 1
let vimclojure#ParenRainbow = 1

" C
set formatoptions=tcqlron
set cinoptions=:0,l1,t0,g0

" OMLet specific (OCaml)
let omlet_indent_match = 0
let omlet_indent_function = 0
let omlet_indent_let = 0
