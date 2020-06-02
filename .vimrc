" .vimrc of Patrick Winter <patrickwinter@posteo.ch>

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'micha/vim-colors-solarized'
Plug 'LnL7/vim-nix'
call plug#end()

" Solarized <3
colorscheme solarized
set background=dark

" Ergnomic replacement for esc
:imap jk <Esc>
:imap kj <Esc>

" leader
let mapleader = " "

" leader mappings
nnoremap <silent> <leader>s :split<CR>
nnoremap <silent> <leader>v :vsplit<CR>
nnoremap <silent> <leader>q :close<CR>

" unsorted
set nocompatible

" indentation
filetype plugin indent on

" expand tabs into spaces
set expandtab

" when using the >> or << commands, shift lines by 4 spaces
set shiftwidth=4
set softtabstop=4

" set tabs to have 4 spaces
set tabstop=4

" indent when moving to the next line while writing code
set autoindent

" spellchecking
"set spell spelllang=de,en

" controls
let mapleader=","
set backspace=indent,eol,start

" disable arrow keys
for prefix in ['i', 'n', 'v']
  for key in ['<Up>', '<Down>', '<Left>', '<Right>']
    exe prefix . "noremap " . key . " <Nop>"
  endfor
endfor

" line numbering
set number
"set relativenumber

" enable syntax highlighting
syntax on

" highlight searches
set hlsearch

" show a visual line under the cursor's current line
" set cursorline

" show the matching part of the pair for [] {} and ()
set showmatch

" Enable folding
set foldmethod=indent
set foldlevel=99
" Enable folding with the spacebar
nnoremap <space> za

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Instead of failing a command because of unsaved changes, instead raise a
" dialogue asking if you wish to save changed files.
set confirm

" Use visual bell instead of beeping when doing something wrong
set visualbell

" Better command-line completion e.g. set colorscheme <Tab>
set wildmenu

" Enforce utf-8
set encoding=utf-8
