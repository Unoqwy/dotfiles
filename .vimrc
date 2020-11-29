source $HOME/.vimrc-fn

set nocompatible
set encoding=utf8

set hidden
set noerrorbells

set lazyredraw
set number relativenumber
set signcolumn=number
set wrap

set mat=2
set magic
set showmatch

set wildmenu
set splitbelow splitright

" tabs
set tabstop=4 softtabstop=4
set shiftwidth=4
set smarttab
set expandtab

" indent
set autoindent
set smartindent

" search
set ignorecase
set smartcase
"set hlsearch
set incsearch

" undo and backup
set undodir=$HOME/.undodir
set undofile
set nobackup
set noswapfile

filetype plugin indent on
syntax on

" one clipboard rule them all
"set clipboard=unnamedplus

""" PLUGINS
call plug#begin('~/.vim/plugged')

" VIM config debug
if !has('nvim')
    Plug 'rhysd/vim-healthcheck'
endif

" Project/file navigation
Plug 'preservim/nerdtree' |
            \ Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'junegunn/fzf.vim'

" Transforms
Plug 'tpope/vim-surround'
Plug 'ntpeters/vim-better-whitespace'

" Completion, syntax, docs, languages, etc
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'preservim/nerdcommenter'
Plug 'tpope/vim-fugitive'

Plug 'rust-lang/rust.vim'
Plug 'cespare/vim-toml'
Plug 'neovimhaskell/haskell-vim'

Plug 'vimwiki/vimwiki'
Plug 'junegunn/goyo.vim'

" Theme/Display
Plug 'itchyny/lightline.vim'
Plug 'gruvbox-community/gruvbox'
"Plug 'ryanoasis/vim-devicons'

call plug#end()
""/ PLUGINS

" Colorscheme
set background=dark
let g:gruvbox_italic=1
colorscheme gruvbox

if exists('+termguicolors')
    " fix to make colors feel right within tmux
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif
set t_Co=256

" Status line
set laststatus=2
set noshowmode
set showcmd

" = gb color red
hi ExtraWhitespace guibg=#fb4934

let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified' ] ],
      \   'right': [ [ 'percent', 'position' ],
      \              [ 'filetype' ],
      \              [ 'fileencoding' ] ]
      \ },
      \ 'component': {
      \   'position': '%l:%c (%L)'
      \ },
      \ }

" Files management
let NERDTreeShowHidden=1

" Writing
let g:vimwiki_list = [{
     \ 'path': '~/vimwiki/',
     \ 'syntax': 'markdown',
     \ 'ext': '.notes.md',
     \ }]

let g:goyo_width  = '60%'
let g:goyo_height = '80%'

set colorcolumn=120
autocmd FileType haskell,cabal set cc=80

" Languages
let g:haskell_indent_disable = 1

let g:coc_global_extensions = [
     \ 'coc-rust-analyzer',
     \ 'coc-toml',
     \ 'coc-tsserver',
     \ 'coc-python',
     \ 'coc-clangd',
     \ 'coc-html',
     \ 'coc-json',
     \ 'coc-xml',
     \ ]

""" KEY BINDINGS
nnoremap <SPACE> <Nop>
let mapleader = " "

" v(im) shortcuts
" R(eload) all
nnoremap <leader>vR :source $HOME/.vimrc<CR>
" r(elaod): c(urrent), p(lugins)
nnoremap <leader>vrc :source %<CR>
nnoremap <leader>vrp :source $HOME/.vimrc<CR>:PlugUpdate<CR>

" f(ile) shortcuts
" l(ist), s(ave)
nnoremap <silent> <leader>fl :NERDTreeToggle<CR>
nnoremap          <leader>fs :w<CR>

nnoremap <silent> <leader>. :Files<CR>
nnoremap <silent><expr> <leader>, ':e#' . v:count . '<CR>'

nnoremap <silent> <leader>W :Goyo<CR>
" mark task as (un)done
nnoremap <silent> <leader>wx F[dt]i[x<ESC>$
nnoremap <silent> <leader>wu F[dt]i[ <ESC>$

" transform bindings
nnoremap <silent>       <leader>tW :StripWhitespace<CR>
nnoremap <silent><expr> <leader>tw  v:count is 0
    \? ':<C-U>set opfunc=<SNR>' . GetScriptNumber('better-whitespace.vim') . '_StripWhitespaceMotion<CR>g@'
    \: ':<C-U>exe ".,+' . v:count . ' StripWhitespace"<CR>'

" COC - Most keybindings here are defaults
" from the README page of coc.nvim as I find them convenient
inoremap <silent><expr> <C-@> coc#refresh()
inoremap <silent><expr> <CR>  pumvisible() ? coc#_select_confirm()
    \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" g[dYir] -> goto code
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" refactoring
nmap <leader>rn <Plug>(coc-rename)

" K in preview -> show doc
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')
""/ KEY BINDINGS

