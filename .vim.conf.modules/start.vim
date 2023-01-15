function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

function! SetupLspBindings()
    "nmap <buffer> <c-]> <plug>(lsp-definition)
    "nmap <buffer> K  <plug>(lsp-hover)
    "nmap <buffer> <F2> <plug>(lsp-rename)

    nmap <buffer> <c-]> <plug>(coc-definition)
    nmap <buffer> K  :call <SID>show_documentation()<CR>
    nmap <buffer> <F2> <plug>(coc-rename)
endfunction

autocmd FileType cpp,c,rust,python,java,haskell,go :call SetupLspBindings()

""https://github.com/snoe/clojure-lsp
""https://github.com/eclipse/eclipse.jdt.ls
""https://github.com/autozimu/LanguageClient-neovim/wiki/Java

" deoplete tab-complete
"inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" new coc.nvim settings
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#confirm():
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice.
inoremap <silent><expr> <c-n> coc#pum#visible() ? coc#pum#next(1)
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')



let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"
