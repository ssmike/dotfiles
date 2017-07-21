" git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
" nmap
" vim -c ":BundleInstall"

set nocp
set viminfo='100,n$HOME/.vim/files/info/viminfo

set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/

call vundle#rc()

filetype plugin indent on

let g:ycm_show_diagnostics_ui = 0

Bundle "Valloric/YouCompleteMe"
Bundle "ludovicchabant/vim-lawrencium"
Bundle "gmarik/vundle"
Bundle "tpope/vim-fugitive"
Bundle "dirkwallenstein/vim-localcomplete"
Bundle "chriskempson/vim-tomorrow-theme"
Bundle "mhinz/vim-startify"
Bundle "ctrlp.vim"
Bundle "bling/vim-airline"
Bundle "easymotion/vim-easymotion"
Bundle "scrooloose/nerdtree"
Bundle "gerw/vim-latex-suite"
Bundle "Tagbar"
Bundle "juneedahamed/vc.vim"
Bundle "eagletmt/neco-ghc"
Bundle "Shougo/vimproc.vim"
Bundle "eagletmt/ghcmod-vim"
Bundle "tpope/vim-salve"
Bundle "clojure-vim/vim-cider"
Bundle "tpope/vim-fireplace"
Bundle "suan/vim-instant-markdown"
Bundle "rking/ag.vim"
Bundle "Chun-Yang/vim-action-ag"
Bundle "glsl.vim"
Bundle "mhinz/vim-signify"
Bundle "gnupg.vim"
Bundle "tpope/vim-rhubarb"
Bundle "tpope/vim-dispatch"
Bundle "rust-lang/rust.vim"
Bundle 'flazz/vim-colorschemes'

"let g:dispatch_handlers = [
"    \ 'tmux',
"   \ 'headless',
"    \ ]

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

let g:instant_markdown_slow = 1

let g:fugitive_git_executable = 'LANG=en git'


"Bundle sirver/ultisnips"
"Bundle honza/vim-snippets"

"Bundle tpope/vim-unimpaired"
"Bundle raimondi/delimitmate"

"let g:UltiSnipsExpandTrigger=<c-w>"

"let g:ctrlp_map='<c-f>'
nmap do :diffget<CR>
nmap dp :diffput<CR>
nmap X :CtrlPBuffer<CR>
nmap <c-f> :CtrlPMRUFiles<CR>
nmap <F3> :qa<CR>
nmap <F4> :bd<CR>

nmap Q <c-w>

Bundle "craigemery/vim-autotag"

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
autocmd! BufRead,BufNewFile *.tex   set makeprg=make
autocmd! BufRead,BufNewFile *.rs 	set filetype=rust
autocmd! BufRead,BufNewFile *.go 	set filetype=go
autocmd! BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl set filetype=glsl

" < ./inp<CR>
"vim-conque"

set wildmode=list:longest,full
set splitright
set splitbelow
"nmap < :vertical resize -6<CR>
"nmap > :vertical resize +6<CR>
nmap <F5> :TagbarToggle<CR>
nmap <F2> :NERDTree<CR>
nmap  GVgg

" for pwd following
"autocmd BufEnter * lcd %:p:h

syn on
"set mapleader = ",""

let g:airline#extensions#tabline#enabled = 1
set laststatus=2

if exists('g:nyaovim_version') "nyaovim
    colorscheme hornet
elseif has('gui_running') "gvim
    colorscheme Tomorrow-Night
    set guifont=Inconsolata\ 13
else
    colorscheme pablo "terminal
endif

if !has("nvim")
    set guiheadroom=0
else
    Bundle 'mklabs/split-term.vim'
endif

map <TAB>p <Plug>(easymotion-prefix)
" <L<TAB>ader>f{char} to move to {char}
map  <TAB>f <Plug>(easymotion-bd-f)
nmap <TAB>f <Plug>(easymotion-overwin-f)

" s{char}{char} to mov<TAB> to {char}{char}
nmap <TAB>s <Plug>(easymotion-overwin-f2)

" Move to line
map <TAB>l <Plug>(easymotion-bd-jk)
nmap <TAB>l <Plug>(easymotion-overwin-line)

" Move to word
map  <TAB>w <Plug>(easymotion-bd-w)
nmap <TAB>w <Plug>(easymotion-overwin-w)

map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
"map ? <Plug>(easymotion-sp)
"omap ? <Plug>(easymotion-tp)

" These `n` & `N` mappings are options. You do not have to map `n` & `N` to EasyMotion.
" Without these mappings, `n` & `N` works fine. (These mappings just provide
" different highlight method and have some other features )
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)
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
let g:syntastic_enable_signs=1

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

nmap <C-t> :GhcModType<CR>
imap <C-t> <C-O>:GhcModType<CR>
" TODO: vmap keybinding
nmap <C-F9> :GhcModCheck<CR>

autocmd BufRead *.gradle setlocal ft=groovy
autocmd BufRead *.hamlet setlocal ft=hamlet
autocmd BufRead *.julius setlocal ft=julius
autocmd BufRead *.cassius setlocal ft=cassius
autocmd BufRead *.lucius setlocal ft=lucius
autocmd BufRead *.dhtml setlocal ft=django

let g:ycm_confirm_extra_conf = 0
let g:syntastic_cpp_compiler='clang++'
let g:syntastic_cpp_compiler_options=' -std=c++11'
if !exists("g:syntastic_c_compiler")
    let g:syntastic_c_compiler="gcc"
endif
if !exists("g:syntastic_c_compiler_options")
    let g:syntastic_c_compiler_options='-ansi -pedantic -Wall'
endif

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
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
map <c-a> 
map! <c-a> 
nmap <c-a> GVgg

imap jj <ESC>
imap ii <ESC>
nmap Q <c-w>

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

""highlight lCursor guifg=NONE guibg=Cyan
set spelllang=ru_yo,en_us

""autocmd FileType text setlocal spell
"
autocmd FileType tex setlocal spell

let g:ycm_filetype_blacklist = {
    \ 'tex' : 1
    \}
function! s:javap()
    setlocal ft=bytecode
    setl readonly nomodified | %!javap -c -s -verbose <afile>
endfunction

au BufReadCmd *.class  call s:javap()
let g:tex_flavor='latex'

"
" YouCompleteMe options
"

let g:ycm_register_as_syntastic_checker = 1 "default 1
let g:Show_diagnostics_ui = 1 "default 1

"will put icons in Vim's gutter on lines that have a diagnostic set.
"Turning this off will also turn off the YcmErrorLine and YcmWarningLine
"highlighting
let g:ycm_enable_diagnostic_signs = 1
let g:ycm_enable_diagnostic_highlighting = 0
let g:ycm_always_populate_location_list = 1 "default 0
let g:ycm_open_loclist_on_ycm_diags = 1 "default 1

let g:ghcmod_ghc_options = ['-fno-warn-missing-signatures']

nmap <c-c> :YcmCompleter GoToDeclaration<CR>
nmap <c-x> :YcmCompleter GoToDefinition<CR>
nmap <c-k> :YcmCompleter GetDoc<CR>
nmap <c-e> :YcmCompleter GoToInclude<CR>

autocmd Filetype c,cpp,python nmap <buffer> <c-]> :YcmCompleter GoToDeclaration<CR>
autocmd FileType c,cpp,objc nnoremap <c-k> :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <c-k> :ClangFormat<CR>
autocmd FileType tex setlocal keymap=russian-jcukenwin

autocmd FileType clojure nmap <buffer> <c-]> [<c-d>

let NERDTreeIgnore = ['\.pyc$']

highlight Pmenu ctermbg=darkgray ctermfg=white
highlight Todo term=reverse ctermbg=1 guibg=DarkRed
highlight Search term=bold,reverse ctermfg=11 ctermbg=12 guifg=#6C6C6C guibg=#ffffff

highlight SignColumn ctermbg=None guibg=None
highlight SignifySignDelete ctermbg=None ctermfg=red
highlight SignifySignAdd ctermbg=None ctermfg=green
highlight SignifySignChange ctermbg=None ctermfg=magenta

au FileType mail let b:delimitMate_autoclose = 0

"match ErrorMsg '\%>80v.\+'

nmap ]] :cn<CR>
nmap [[ :cp<CR>
let g:ycm_python_binary_path='python'

"ya vim doesn't see my x session
imap <c-v> <c-r>=system('xsel -b')<cr>

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

let g:tex_conceal = ""

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
