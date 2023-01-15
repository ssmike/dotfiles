Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'tpope/vim-dispatch', {'for': 'clojure'}
Plug 'tpope/vim-salve', {'for': 'clojure'}
Plug 'tpope/vim-fireplace', {'for': 'clojure'}
au FileType clojure nmap <buffer> <c-]> ]<c-d>

"let g:deoplete#enable_at_startup = 1
"if has('nvim')
"  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"else
"  Plug 'Shougo/deoplete.nvim'
"  Plug 'roxma/nvim-yarp'
"  Plug 'roxma/vim-hug-neovim-rpc'
"endif

"Plug 'prabirshrestha/vim-lsp'
"Plug 'lighttiger2505/deoplete-vim-lsp'
"Plug 'mattn/vim-lsp-settings'
