"completion function for plain text files"

" for all plugins work vim must be compiled with +python
" from ubuntu forums : remove vim-tiny and install vim-nox
" for ubuntu - vim-nox package
" vundle created by git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
" " installation instructions :
" git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
" vim -c ":BundleUpdate"
" for python and c-family languages completion
"   read README on https://github.com/Valloric/YouCompleteMe or
"   (if you have boost and clang >= 3.4 installed) just run
"   cd ~/.vim/bundle/YouCompleteMe; mkdir ycm_build; cd ycm_build; cmake .. ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp -DUSE_SYSTEM_BOOST=ON; make
"   or (with bundled libs) cd ~/.vim/bundle/YouCompleteMe; ./install.sh  --clang-completer
" for haskell:
"   install ghc-mod
"   vimproc build required - cd ~/.vim/bundle/vimproc.vim && make

set guiheadroom=0

"for project configuration"
if filereadable(".vim.local")
    so .vim.local
endif

set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/

call vundle#rc()

filetype plugin indent on

let g:UltiSnipsSnippetDirectories=["UltiSnips"]
let g:UltiSnipsExpandTrigger="<c-k>"


Bundle 'Valloric/YouCompleteMe'
let g:ycm_show_diagnostics_ui = 0

Bundle "gmarik/vundle"
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle "pbrisbin/vim-syntax-shakespeare"

Bundle 'javacomplete'
Bundle "dirkwallenstein/vim-localcomplete"
Bundle "chriskempson/vim-tomorrow-theme"
Bundle 'Tagbar'
Bundle "acx0/Conque-Shell"
Bundle 'The-NERD-Commenter'
Bundle "Shougo/vimproc.vim"
Bundle "eagletmt/ghcmod-vim"
Bundle "eagletmt/neco-ghc"
"Bundle 'lervag/vim-latex'
Bundle "gerw/vim-latex-suite"
set rtp+=~/.vim/bundle/vim-latex-suite
Bundle "mhinz/vim-startify"
Bundle "ctrlp.vim"
Bundle 'fugitive.vim'
let g:fugitive_git_executable = 'LANG=en git'
let g:ctrlp_map='<c-f>'
nmap <c-x> :CtrlPBuffer<CR>
nmap <c-n> :bn<CR>
nmap <c-p> :bp<CR>

Bundle "wting/rust.vim"
Bundle "groovy.vim"
Bundle "craigemery/vim-autotag"

Bundle "rhysd/vim-clang-format"

set tabstop=2
set shiftwidth=2
set smarttab
set expandtab

autocmd FileType make setlocal noexpandtab

set autoread
nmap <TAB> :tabn<CR>
nmap <BACKSPACE> :tabp<CR>
nmap <F2> :tabnew<CR>
nmap <F3> :q!<CR>
nmap s :w<CR>

set autoindent
imap {<CR>  {<CR>}<LEFT><CR><UP><TAB>
imap {<SPACE>   {}<LEFT>
set number

filetype plugin on
autocmd! BufRead,BufNewFile *.rs 	set filetype=rust

"templates api"
nmap <F9> :make<CR>
nmap <F8> :!make run<CR>

" < ./inp<CR>
"vim-conque"
function! Term()
    ConqueTerm zsh
    nmap <F3> :q!<CR>
    nmap <F8> :!make run<CR>
    nmap <F2> :tabnew<CR>
endfunction
nmap t :call Term()<CR>

set wildmode=list:longest,full
nmap e :Explore<CR>
nmap ct :vertical split<CR>
nmap vt :split<CR>
set splitright
set splitbelow
nmap < :vertical resize -6<CR>
nmap > :vertical resize +6<CR>
nmap <F4> :NERDTree<CR>
nmap <F5> :TagbarToggle<CR>
nmap  GVgg

" for pwd following
"autocmd BufEnter * lcd %:p:h

syn on
"set mapleader = ",""

Bundle "bling/vim-airline"
let g:airline#extensions#tabline#enabled = 1
set laststatus=2

if has('gui_running')
    colorscheme Tomorrow-Night
    set guifont=Inconsolata\ 13
else
    colorscheme pablo
endif

" tool menu
set guioptions-=T
" main menu
set guioptions-=m
" tab appearance
set guioptions-=e

set completeopt=menu,menuone
let g:syntastic_enable_signs=1

nmap S :SyntasticToggleMode<CR>

imap <c-f> <c-x><c-f>

" working with folds
nmap <leader>s :set fdm=syntax<CR>
nmap <leader><TAB> :set fdm=indent<CR>
nmap <leader>o zo
nmap <leader>c zc
nmap <leader>m :set fdm=manual<CR>

set completeopt-=preview
set splitbelow

autocmd Filetype java setlocal completefunc=javacomplete#Complete
""autocmd Filetype java setlocal omnifunc=javacomplete#Complete

autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd FileType haskell setlocal completefunc=necoghc#omnifunc

nmap <C-t> :GhcModType<CR>
imap <C-t> <C-O>:GhcModType<CR>
" TODO: vmap keybinding
nmap <C-F9> :GhcModCheck<CR>
nmap <C-u> :GhcModInfoPreview<CR>
imap <C-u> <C-O>:GhcModInfoPreview<CR>

autocmd BufRead *.gradle setlocal ft=groovy
autocmd BufRead *.hamlet setlocal ft=hamlet
autocmd BufRead *.julius setlocal ft=julius
autocmd BufRead *.cassius setlocal ft=cassius
autocmd BufRead *.lucius setlocal ft=lucius

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
vmap . "+
nmap . "+

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

function! Includefunction(param)
    exe 'normal! ggO#include '.a:param
    exe 'normal ``'
endfunction

command! -nargs=1 Include call Includefunction('<args>')

set tags=./tags;/

imap jj <ESC>
map J gt
map K gT
set timeoutlen=300
set mouse=a

""" for tex
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
map  
map!  
nmap  GVgg
""highlight lCursor guifg=NONE guibg=Cyan
set spelllang=ru_yo,en_us

let g:syntastic_tex_checkers=['']
"let b:atpTexCompiler='texi2pdf'
""autocmd FileType text setlocal spell
"let g:atp_tab_map=1
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
let g:ycm_semantic_triggers = {'haskell' : ['.']}

let g:ghcmod_ghc_options = ['-fno-warn-missing-signatures']

nmap <c-c> :YcmCompleter GoToDeclaration<CR>

let g:clang_format#code_style='google'
autocmd FileType c,cpp,objc nnoremap <c-k> :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <c-k> :ClangFormat<CR>
autocmd FileType tex set keymap=russian-jcukenwin
"let g:clang_format#auto_format=1
