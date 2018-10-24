if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

if has('nvim')
    set rtp+=/usr/share/vim/vimfiles,~/.vim/
endif

set nocp
set viminfo='100,n$HOME/.vim/files/info/viminfo

set nocompatible
filetype off
set title

let g:deoplete#enable_at_startup = 1

call plug#begin('~/.vim/plugged')
    if has('nvim')
      Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    else
      Plug 'Shougo/deoplete.nvim'
      Plug 'roxma/nvim-yarp'
      Plug 'roxma/vim-hug-neovim-rpc'
    endif

    Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': 'bash install.sh'
        \ }

    Plug 'LnL7/vim-nix', {'for': 'nix'}

    Plug 'tpope/vim-fugitive'
    Plug 'gregsexton/gitv', {'on': 'Gitv'}
    Plug 'juneedahamed/vc.vim'
    Plug 'tpope/vim-rhubarb'
    Plug 'mhinz/vim-signify'

    if has('python3')
        if has('nvim')
            Plug 'Shougo/denite.nvim', {'do': ':UpdateRemotePlugins'}
        else
            Plug 'Shougo/denite.nvim'
        endif
    else
        Plug 'rking/ag.vim'
        Plug 'Chun-Yang/vim-action-ag'
        Plug 'ctrlpvim/ctrlp.vim'
    endif

    Plug 'gerw/vim-latex-suite', {'for': 'tex'}
    Plug 'jamessan/vim-gnupg'

    Plug 'rust-lang/rust.vim', {'for': 'rust'}

    Plug 'l04m33/vlime', {'rtp': 'vim/', 'for': 'lisp'}
    Plug 'mhinz/vim-startify'

    Plug 'flazz/vim-colorschemes'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'luochen1990/rainbow', {'for': ['clojure', 'lisp']}

    Plug 'tpope/vim-dispatch', {'for': 'clojure'}
    Plug 'tpope/vim-salve', {'for': 'clojure'}
    Plug 'tpope/vim-fireplace', {'for': 'clojure'}

    Plug 'majutsushi/tagbar'

    Plug 'craigemery/vim-autotag'

    if !has("nvim")
        set guiheadroom=0
    else
        Plug 'mklabs/split-term.vim'
    endif
    Plug 'chriskempson/vim-tomorrow-theme'
call plug#end()

au FileType clojure nmap <buffer> <c-]> ]<c-d>

function SetupLspBindings()
    noremap <buffer> <c-]> :call LanguageClient#textDocument_definition()<CR>
    nnoremap <buffer> K :call LanguageClient#textDocument_hover()<CR>
    nnoremap <buffer> <F2> :call LanguageClient#textDocument_rename()<CR>
endfunction

autocmd FileType cpp,c,rust,python,java,haskell :call SetupLspBindings()

let maplocalleader = ","
let mapleader = " "

let g:LanguageClient_serverCommands = {
  \ 'rust': ['rls'],
  \ 'cpp': ['clangd'],
  \ 'python': ['pyls'],
  \ 'java': ['jdt.ls'],
  \ 'haskell': ['hie-wrapper']
  \ }

""https://github.com/snoe/clojure-lsp
""https://github.com/eclipse/eclipse.jdt.ls
""https://github.com/autozimu/LanguageClient-neovim/wiki/Java

let g:LanguageClient_autoStart = 1

" deoplete tab-complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

filetype plugin indent on

autocmd! BufRead,BufNewFile *.nix    set filetype=nix

"is disabled in LargeFile
nmap <leader>t :TagbarToggle<CR>

let g:rainbow_active = 1
let g:rainbow_conf = {
    \   'guifgs': ['SeaGreen3', 'DarkOrchid3', 'firebrick3', 'RoyalBlue3', 'SeaGreen3', 'DarkOrchid3', 'firebrick3', 'RoyalBlue3', 'SeaGreen3', 'DarkOrchid3', 'firebrick3', 'RoyalBlue3', 'RoyalBlue3', 'SeaGreen3', 'DarkOrchid3'],
    \   'ctermfgs': ['Darkblue','darkgray','darkgreen','darkcyan','darkred','darkmagenta','brown','gray','darkmagenta','Darkblue','brown','darkgreen','darkcyan','darkred'],
    \   'operators': '_,_',
    \   'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
    \   'separately' : {
    \       '*': 0,
    \       'clojure': {},
    \       'lisp': {}
    \   }
    \}

let g:vc_browse_cache_all = 1

let g:fugitive_github_domains=['github.yandex-team.ru']

let g:fugitive_git_executable = 'LANG=en git'

nmap do :diffget<CR>
nmap dp :diffput<CR>

nmap <F3> :qa<CR>
nmap <F4> :bd<CR>

nmap <leader>w <c-w>

set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
"set formatoptions+=w
"set tw=80
"nnoremap Q gqip
autocmd FileType make setlocal noexpandtab

set autoread
nmap <TAB> :bn<CR>
nmap <BACKSPACE> :bp<CR>
nmap s :w<CR>

set autoindent
imap {<CR>  {<CR>}jjO
imap {<SPACE>   {}<LEFT>

set number
set relativenumber

filetype plugin on

autocmd! BufRead,BufNewFile *.rs    set filetype=rust
autocmd! BufRead,BufNewFile *.go    set filetype=go
autocmd! BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl set filetype=glsl

" < ./inp<CR>
"vim-conque"

set wildmode=list:longest,full
set splitright
set splitbelow
"nmap < :vertical resize -6<CR>
"nmap > :vertical resize +6<CR>
nmap <F2> :NERDTree<CR>
nmap  GVgg

" for pwd following
"autocmd BufEnter * lcd %:p:h

syn on

let g:airline#extensions#tabline#enabled = 1
set laststatus=2
if exists('g:GtkGuiLoaded')
    colorscheme molokai
    call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0)
elseif has('gui_running') "gvim
    colorscheme Tomorrow-Night
    set guifont=Inconsolata\ 10
    let g:airline_theme='raven'
else
    colorscheme pablo "terminal
    let g:airline_theme='raven'
endif

set nohlsearch

set autochdir

" tool menu
set guioptions-=T
" main menu
set guioptions-=m
" tab appearance
set guioptions-=e
" scrollbars
set guioptions-=r
set guioptions-=R

set completeopt=menu,menuone

imap <c-f> <c-x><c-f>

" working with folds
nmap `s :set fdm=syntax<CR>
nmap `<TAB> :set fdm=indent<CR>
nmap `m :set fdm=manual<CR>


set completeopt-=preview
set splitbelow


autocmd Filetype java setlocal completefunc=javacomplete#Complete
""autocmd Filetype java setlocal omnifunc=javacomplete#Complete

autocmd BufRead *.gradle setlocal ft=groovy
autocmd BufRead *.hamlet setlocal ft=hamlet
autocmd BufRead *.julius setlocal ft=julius
autocmd BufRead *.cassius setlocal ft=cassius
autocmd BufRead *.lucius setlocal ft=lucius
autocmd BufRead *.dhtml setlocal ft=django

"x11 clipboard"
"vmap . "+
"nmap . "+

nnoremap <A-1> 1gt
nnoremap <A-2> 2gt
nnoremap <A-3> 3gt
nnoremap <A-4> 4gt
nnoremap <A-5> 4gt
nnoremap <A-q> 5gt
nnoremap <A-w> 6gt
nnoremap <A-e> 7gt
nnoremap <A-r> 8gt

command Print !gtklp %

function! DisplayHiddenCharacters()
    set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣,nbsp:+
    set list
endfunction

function! HideHiddenCharacters()
    set nolist
endfunction

command! Hide call HideHiddenCharacters()
command! Show call DisplayHiddenCharacters()

function! Includefunction(param)
    exe 'normal! ggO#include '.a:param
    exe 'normal ``'
endfunction

command! -nargs=1 Include call Includefunction('<args>')

set tags=./tags;/

set timeoutlen=300
set mouse=a

""" for tex
function! DualLangMode()
    setlocal keymap=russian-jcukenwin
    map <buffer> <c-a> 
    map! <buffer> <c-a> 
endfunction

command Ru :call DualLangMode()

""highlight lCursor guifg=NONE guibg=Cyan
set spelllang=ru_yo,en_us

autocmd! BufRead,BufNewFile *.tex   setlocal makeprg=make

let g:tex_flavor='latex'

function TexMode()
    let g:tex_conceal = ""
    setlocal iminsert=0
    setlocal imsearch=0
    setlocal spell
    call DualLangMode()
endfunction

autocmd FileType tex call TexMode()
autocmd FileType mail call DualLangMode()

nmap <c-a> GVgg
imap jj <ESC>

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

function! s:javap()
    setlocal ft=bytecode
    setl readonly nomodified | %!javap -c -s -verbose <afile>
endfunction

au BufReadCmd *.class  call s:javap()

"
" YouCompleteMe options
"

let g:Show_diagnostics_ui = 1 "default 1

let g:ghcmod_ghc_options = ['-fno-warn-missing-signatures']

"nmap <c-k> :YcmCompleter GetDoc<CR>

" more convinient tag jump bindings for me
nnoremap g] g<c-]>
nnoremap <leader>p :pop<CR>

let NERDTreeIgnore = ['\.pyc$']

highlight StatusLine ctermfg=188 ctermbg=235 guifg=#c8c8c8 guibg=#2e2e2e
highlight MsgSeparator ctermfg=188 ctermbg=235 guifg=#c8c8c8 guibg=#2e2e2e
highlight Pmenu ctermbg=darkgray ctermfg=white guibg=#6c6c6c guifg=white
highlight Todo term=reverse ctermbg=1 guibg=DarkRed
highlight Search term=bold,reverse ctermfg=0 ctermbg=11 guifg=Black guibg=Yellow

silent! highlight SignColumn ctermbg=None guibg=None
highlight SignifySignDelete ctermbg=None ctermfg=red
highlight SignifySignAdd ctermbg=None ctermfg=green
highlight SignifySignChange ctermbg=None ctermfg=magenta

au FileType mail let b:delimitMate_autoclose = 0

nmap ]] :cn<CR>
nmap [[ :cp<CR>
nmap )) :lnext<CR>
nmap (( :lprevious<CR>

function Extcommand(...)
    normal i<c-r>=system(\'a:000\')<cr>
endfunction

let parent=1
let local_vimrc = ".vim.local"
while parent <= 40
    if filewritable(local_vimrc)
        exe ":so " . local_vimrc
    endif
    let local_vimrc = "../". local_vimrc
    let parent = parent+1
endwhile
unlet parent local_vimrc


autocmd! bufwritepost ~/.vimrc execute "normal! :source ~/.vimrc"

function! SwitchSourceHeader()
  "update!
  if (expand ("%:e") == "cpp")
    find %:t:r.h
  else
    find %:t:r.cpp
  endif
endfunction

au FileType c,cpp nmap <buffer> <leader>s :call SwitchSourceHeader()<CR>

let g:ag_prg="rg --vimgrep --smart-case"

autocmd! BufRead,BufNewFile *.rs    setlocal makeprg=cargo

" file is large from 100K
let g:LargeFile = 1024 * 300
augroup LargeFile
 autocmd BufReadPre * let f=getfsize(expand("<afile>")) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
augroup END

function LargeFile()
 let b:tagbar_ignore = 1
 " display message
 autocmd VimEnter *  echo "The file larger than " . (g:LargeFile / 1024) . " KB, some plugins are disabled."
endfunction

let g:vc_allow_leader_mappings=0

au FileType mail setl fo+=awq
au FileType mail setl wm=4

if has('python3')
    nmap <leader>b :Denite -auto-resize buffer<CR>
    nmap <leader>f :Denite -auto-resize file<CR>
    nmap gw :DeniteCursorWord -mode=normal -auto-resize grep<CR>
    command! -nargs=1 Ag :Denite -mode=normal -auto-resize grep -input='<args>'
    command! Fix :Denite -mode=normal -auto-resize codeAction
    nmap <leader>r :Denite -auto-resize register<CR>
    call denite#custom#map('insert', 'jj', '<denite:enter_mode:normal>', 'noremap')
else
    nmap <leader>b :CtrlPBuffer<CR>
    nmap <leader>f :CtrlPMRUFiles<CR>
    nmap gw gagiw
endif

nmap <SPACE>] <C-]>
nmap <SPACE>[ <C-o>
