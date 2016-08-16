set guiheadroom=0

set nocompatible
filetype off

filetype plugin indent on

nmap do :diffget<CR>
nmap dp :diffput<CR>
nmap <c-n> :bn<CR>
nmap <c-p> :bp<CR>

set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
"set formatoptions+=w
"set tw=80
nnoremap Q gqip
autocmd FileType make setlocal noexpandtab

set autoread
nmap <TAB> :tabn<CR>
nmap <BACKSPACE> :tabp<CR>
nmap s :w<CR>

set autoindent
imap {<CR>  {<CR>}<LEFT><CR><UP><TAB>
imap {<SPACE>   {}<LEFT>

set number
set relativenumber

filetype plugin on
autocmd! BufRead,BufNewFile *.rs 	set filetype=rust
autocmd! BufRead,BufNewFile *.go 	set filetype=go

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
set laststatus=2

if has('gui_running')
    colorscheme Tomorrow-Night
    set guifont=Inconsolata\ 13
else
    colorscheme pablo
endif

set autochdir

" tool menu
set guioptions-=T
" main menu
set guioptions-=m
" tab appearance
set guioptions-=e

set completeopt=menu,menuone

imap <c-f> <c-x><c-f>

" working with folds
nmap `s :set fdm=syntax<CR>
nmap `<TAB> :set fdm=indent<CR>
nmap `m :set fdm=manual<CR>


set completeopt-=preview
set splitbelow

autocmd BufRead *.gradle setlocal ft=groovy
autocmd BufRead *.hamlet setlocal ft=hamlet
autocmd BufRead *.julius setlocal ft=julius
autocmd BufRead *.cassius setlocal ft=cassius
autocmd BufRead *.lucius setlocal ft=lucius
autocmd BufRead *.dhtml setlocal ft=django

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

set timeoutlen=300

""" for tex
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
map <c-a> 
map! <c-a> 
nmap <c-a> GVgg

"true vimer
imap jj <ESC>
imap ii <ESC>
imap iw <c-w>
nmap Q <c-w>
"inoremap <Up> <NOP>
inoremap <Down> <NOP>
"inoremap <Left> <NOP>
inoremap <Right> <NOP>
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

""highlight lCursor guifg=NONE guibg=Cyan
set spelllang=ru_yo,en_us

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

autocmd FileType tex set keymap=russian-jcukenwin
"let g:clang_format#auto_format=1

let NERDTreeIgnore = ['\.pyc$']
highlight Pmenu ctermbg=darkgray ctermfg=white
highlight Todo term=reverse ctermbg=1 guibg=DarkRed
highlight Search term=bold,reverse ctermfg=11 ctermbg=12 guifg=#ffff00 guibg=#0000ff
highlight StatusLine ctermfg=white ctermbg=darkgrey

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
