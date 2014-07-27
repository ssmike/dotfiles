
"completion function for plain text files"

" for all plugins work vim must be compiled with +python 
" from ubuntu forums : remove vim-tiny and install vim-nox
" for ubuntu - vim-nox package
" vundle created by git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle 
" installation instructions : 
" git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
" for python jedi complete 
" install python-jedi: 
"   arch : pacman -S python-jedi [python2-jedi] 
"   gentoo : emerge dev-python/jedi 
"   ubuntu : apt-get install python-jedi python3-jedi 
" for c++ clang complete 
" install clang 
"   arch : pacman -S clang 
"   gentoo : unmask and emerge sys-devel/clang 
"   ubuntu : apt-get install clang 
" for haskell:
"   install ghc-mod 
"   vimproc build required - cd ~/.vim/bundle/vimproc.vim && make
" for editing java files jdk needed
" final step : vim -c ":BundleUpdate"

"for project configuration"
if filereadable(".vim.local")
    so .vim.local
endif

set nocompatible
filetype off  

set rtp+=~/.vim/bundle/vundle/

call vundle#rc()

filetype plugin indent on   
Bundle "gmarik/vundle"
Bundle 'scrooloose/nerdtree'
Bundle 'clang-complete'
"vundle compatibility broken
"Bundle "scrooloose/syntastic"
Bundle "pbrisbin/vim-syntax-shakespeare"
Bundle 'davidhalter/jedi-vim'
Bundle "SuperTab"
Bundle "javacomplete"
Bundle "dirkwallenstein/vim-localcomplete"
Bundle "chriskempson/vim-tomorrow-theme"
Bundle "Tagbar"
Bundle "acx0/Conque-Shell"
Bundle 'The-NERD-Commenter'

Bundle "Shougo/vimproc.vim"
Bundle "eagletmt/ghcmod-vim"
Bundle "eagletmt/neco-ghc"

Bundle "mhinz/vim-startify"

set tabstop=4
set shiftwidth=4
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
imap {<CR>  {<CR>}<LEFT><CR><UP><SPACE><SPACE><SPACE><SPACE>
imap {<SPACE>   {}<LEFT>
set number

filetype plugin on

"templates api"
nmap <F9> :make<CR>
nmap <F8> :make run<CR>

" < ./inp<CR>
"vim-conque"
function! Term()
    ConqueTerm zsh
    nmap <F8> :!./main<CR>
    nmap <F3> :q!<CR>
    nmap <F2> :tabnew<CR>
endfunction
nmap t :call Term()<CR>

set wildmode=list:longest,full
nmap e :Explore<CR>
nmap ct :vertical split<CR>
nmap vt :split<CR>
nmap < :vertical resize -6<CR>
nmap > :vertical resize +6<CR>
nmap <F4> :NERDTree<CR>
nmap [ :resize -5<CR>
nmap ] :resize +5<CR>
nmap <F5> :TagbarToggle<CR>

" for pwd following
"autocmd BufEnter * lcd %:p:h

syn on
"set mapleader = ",""
colorscheme Tomorrow-Night

" tool menu
set guioptions-=T
" main menu
set guioptions-=m
" tab appearance
set guioptions-=e

set completeopt=menu,menuone
let g:syntastic_enable_signs=1

nmap S :SyntasticToggleMode<CR>
set completefunc=localcomplete#allBufferMatches

imap <c-f> <c-x><c-f>

" working with folds
nmap <leader>s :set fdm=syntax<CR>
nmap <leader><TAB> :set fdm=indent<CR>
nmap <leader>o zo
nmap <leader>c zc
nmap <leader>m :set fdm=manual<CR>

"for python completion"
set completeopt-=preview
set splitbelow
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

let g:SuperTabDefaultCompletionType = "<c-x><c-u>"
autocmd FileType python setlocal completefunc=jedi#completions

autocmd Filetype java setlocal completefunc=javacomplete#Complete
autocmd Filetype java setlocal omnifunc=javacomplete#Complete

autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd FileType haskell setlocal completefunc=necoghc#omnifunc

nmap <C-t> :GhcModType<CR>
imap <C-t> <C-O>:GhcModType<CR>
" TODO: vmap keybinding
nmap <C-F9> :GhcModCheck<CR>
nmap <C-u> :GhcModInfoPreview<CR>
imap <C-u> <C-O>:GhcModInfoPreview<CR>

"let g:SuperTabContextTextPrecedence = ['&omnifunc', '&completefunc']"

"local configuration for clang_complete

if !exists("g:clang_user_options") 
    let g:clang_user_options="-std=c++0x"
endif
"let g:clang_user_options="-std=c++0x"
if !exists("g:syntastic_cpp_compiler")
    let g:syntastic_cpp_compiler='clang++'
endif
if !exists("g:syntastic_cpp_compiler_options")
    let g:syntastic_cpp_compiler_options=' -std=c++11'
endif
if !exists("g:syntastic_c_compiler")
    let g:syntastic_c_compiler="gcc"
endif
if !exists("g:syntastic_c_compiler_options")
    let g:syntastic_c_compiler_options='-ansi -pedantic -Wall'
endif

"x11 clipboard"
nmap [p "+p

