if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

if has('nvim')
    set rtp+=/usr/share/vim/vimfiles
endif

set nocp
set viminfo='100,n$HOME/.vim/files/info/viminfo

set nocompatible
filetype off
set title

call plug#begin('~/.vim/plugged')
    Plug 'Valloric/YouCompleteMe', {'do': 'python ./install.py --clang-completer --gocode-completer --racer-completer'}
    Plug 'LnL7/vim-nix'
    Plug 'artur-shaik/vim-javacomplete2'

    Plug 'tpope/vim-fugitive'
    Plug 'gregsexton/gitv'
    Plug 'juneedahamed/vc.vim'
    Plug 'tpope/vim-rhubarb'
    Plug 'mhinz/vim-signify'

    Plug 'bitc/vim-hdevtools'

    Plug 'Shougo/denite.nvim'
    Plug 'itchyny/vim-cursorword'

    Plug 'tpope/vim-salve'
    Plug 'clojure-vim/vim-cider'
    Plug 'tpope/vim-fireplace'

    Plug 'dirkwallenstein/vim-localcomplete'

    Plug 'gerw/vim-latex-suite'
    Plug 'jamessan/vim-gnupg'
    Plug 'tpope/vim-dispatch'

    Plug 'rust-lang/rust.vim'

    Plug 'l04m33/vlime', {'rtp': 'vim/'}
    Plug 'tpope/vim-db'
    Plug 'mhinz/vim-startify'

    Plug 'flazz/vim-colorschemes'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'luochen1990/rainbow'

    if exists('g:nyaovim_version')
        Plug 'rhysd/nyaovim-popup-tooltip'
        Plug 'rhysd/nyaovim-markdown-preview'
        Plug 'rhysd/nyaovim-mini-browser'
        nnoremap <Leader>o :<C-u>MiniBrowser <C-r><C-p><CR>
    else
        Plug 'suan/vim-instant-markdown'
        let g:instant_markdown_slow = 1
    endif

    Plug 'majutsushi/tagbar'

    Plug 'craigemery/vim-autotag'

    if !has("nvim")
        set guiheadroom=0
    else
        Plug 'mklabs/split-term.vim'
    endif
    Plug 'chriskempson/vim-tomorrow-theme'
call plug#end()

filetype plugin indent on

autocmd! BufRead,BufNewFile *.nix    set filetype=nix

au Filetype haskell :noremap <buffer> <c-t> :HdevtoolsType<CR>
au Filetype haskell :noremap <buffer> <c-c> :HdevtoolsClear<CR>

let g:ale_linters = {
            \   'cpp': [],
            \   'python': ['pylint']
            \}

"is disabled in LargeFile
nmap t :TagbarToggle<CR>

let maplocalleader = "\<Space>"

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

let g:cider_no_maps=1 " Disable built-in mappings
"just maps from set_up without <F4>-<F5>
nmap <buffer> cf <Plug>CiderFormat
nmap <buffer> cff <Plug>CiderCountFormat
nmap <buffer> cF ggcfG
nmap <buffer> cdd <Plug>CiderUndef
nmap <buffer> cn <Plug>RefactorCleanNs
nmap <buffer> cRR <Plug>RefactorResolveMissing
nmap <buffer> cfs <Plug>RefactorFindSymbol

let g:vc_browse_cache_all = 1

let g:fugitive_github_domains=['github.yandex-team.ru']

let g:fugitive_git_executable = 'LANG=en git'

nmap do :diffget<CR>
nmap dp :diffput<CR>

nmap <F3> :qa<CR>
nmap <F4> :bd<CR>

nmap Q <c-w>

set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
"set formatoptions+=w
"set tw=80
nnoremap Q gqip
autocmd FileType make setlocal noexpandtab

set autoread
nmap <TAB> :bn<CR>
nmap <BACKSPACE> :bp<CR>
nmap s :w<CR>

set autoindent
imap {<CR>  {<CR>}iiO
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

if exists('g:nyaovim_version') "nyaovim
    colorscheme stackoverflow
    let g:airline_theme='sol'
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

autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd FileType haskell setlocal completefunc=necoghc#omnifunc
let g:ycm_semantic_triggers = {'haskell' : ['.'], 'clojure' : ['/', '.']}

autocmd BufRead *.gradle setlocal ft=groovy
autocmd BufRead *.hamlet setlocal ft=hamlet
autocmd BufRead *.julius setlocal ft=julius
autocmd BufRead *.cassius setlocal ft=cassius
autocmd BufRead *.lucius setlocal ft=lucius
autocmd BufRead *.dhtml setlocal ft=django

let g:ycm_confirm_extra_conf = 0

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

function! YDocFunction()
    exe ':YcmCompleter GetDoc'
endfunction

command! YDoc call YDocFunction()

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

let g:ycm_filetype_blacklist = {
    \ 'tex' : 1
    \}

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
imap ii <ESC>
nmap <SPACE> <c-w>

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

let g:ycm_register_as_syntastic_checker = 0 "default 1
let g:Show_diagnostics_ui = 1 "default 1
let g:ycm_show_diagnostics_ui = 1

"will put icons in Vim's gutter on lines that have a diagnostic set.
"Turning this off will also turn off the YcmErrorLine and YcmWarningLine
"highlighting
let g:ycm_enable_diagnostic_signs = 1
let g:ycm_enable_diagnostic_highlighting = 1
let g:ycm_always_populate_location_list = 1 "default 0
let g:ycm_open_loclist_on_ycm_diags = 1 "default 1

let g:ghcmod_ghc_options = ['-fno-warn-missing-signatures']

nmap <c-k> :YcmCompleter GetDoc<CR>

autocmd Filetype c,cpp,python,rust nmap <buffer> <c-]> :YcmCompleter GoTo<CR>
command YFix YcmCompleter FixIt

autocmd FileType clojure nmap <buffer> <c-]> [<c-d>

let NERDTreeIgnore = ['\.pyc$']

highlight Pmenu ctermbg=darkgray ctermfg=white
highlight Todo term=reverse ctermbg=1 guibg=DarkRed
highlight Search term=bold,reverse ctermfg=11 ctermbg=12 guifg=#6C6C6C guibg=#ffffff

silent! highlight SignColumn ctermbg=None guibg=None
highlight SignifySignDelete ctermbg=None ctermfg=red
highlight SignifySignAdd ctermbg=None ctermfg=green
highlight SignifySignChange ctermbg=None ctermfg=magenta

au FileType mail let b:delimitMate_autoclose = 0

nmap ]] :cn<CR>
nmap [[ :cp<CR>
let g:ycm_python_binary_path='python'

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

nmap ,s :call SwitchSourceHeader()<CR>

let g:ag_prg="rg --vimgrep --smart-case"

let g:ycm_rust_src_path = '~/.local/rust/src/'
autocmd! BufRead,BufNewFile *.rs    setlocal makeprg=cargo

" file is large from 100K
let g:LargeFile = 1024 * 100
augroup LargeFile
 autocmd BufReadPre * let f=getfsize(expand("<afile>")) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
augroup END

function LargeFile()
 let b:tagbar_ignore = 1
 let b:ale_linters = []
 " display message
 autocmd VimEnter *  echo "The file larger than " . (g:LargeFile / 1024) . " KB, some plugins are disabled."
endfunction

let g:vc_allow_leader_mappings=1

au FileType mail setl fo+=awq
au FileType mail setl wm=4

nmap <SPACE>b :Denite -auto-resize buffer<CR>
nmap <SPACE>f :Denite -auto-resize file<CR>
nmap gw :DeniteCursorWord -auto-resize grep<CR>
command! -nargs=1 Ag :Denite -auto-resize grep -input='<args>'
nmap <SPACE>r :Denite -auto-resize register<CR>
call denite#custom#map('insert', 'jj', '<denite:enter_mode:normal>', 'noremap')
