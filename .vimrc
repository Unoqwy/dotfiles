set nocompatible encoding=utf8
set noerrorbells
set hidden

set lazyredraw
set number relativenumber
set signcolumn=number
set wrap linebreak
set noshowmatch

set magic

set wildmenu

" tabs
set tabstop=4 softtabstop=4
set shiftwidth=4
set smarttab expandtab

" indent
set autoindent
set smartindent

" search
set ignorecase smartcase
set incsearch

" undo and backup
set undodir=$HOME/.undodir undofile
set nobackup noswapfile

filetype plugin indent on
syntax on

" I often find myself wanting to switch colorscheme
" so I have this easy want of doing so between the 3 themes I used
" > 1: gruvbox | 2: miramare | 3: ayu-mirage
let s:theme=3

""" PLUGINS
call plug#begin('~/.vim/plugged')

" Project/file navigation
Plug 'junegunn/fzf.vim'
Plug 'preservim/tagbar'

" Transforms
Plug 'tpope/vim-surround'
Plug 'ntpeters/vim-better-whitespace'

" Completion, syntax, docs, languages, git, etc
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'SirVer/ultisnips'
Plug 'preservim/nerdcommenter'

Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" the best
Plug 'rust-lang/rust.vim'
Plug 'cespare/vim-toml'
Plug 'neovimhaskell/haskell-vim'

" web
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript'
Plug 'nelsyeung/twig.vim'

" Writing
Plug 'vimwiki/vimwiki'
Plug 'junegunn/goyo.vim'

" Theme/Display
if s:theme == 1
    Plug 'gruvbox-community/gruvbox'
elseif s:theme == 2
    Plug 'franbach/miramare'
elseif s:theme == 3
    Plug 'ayu-theme/ayu-vim'
endif

Plug 'itchyny/lightline.vim'
Plug 'jszakmeister/vim-togglecursor'

call plug#end()
""/ PLUGINS

""" THEME
set termguicolors t_Co=256
if exists('+termguicolors')
    " fix to make colors feel right within tmux
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

set background=dark
if s:theme == 1
    let g:gruvbox_italic=1
    let s:lightline_theme="seoul256"
    colorscheme gruvbox
elseif s:theme == 2
    let g:miramare_enable_italic=1
    let g:miramare_enable_italic_string=1
    let g:miramare_enable_bold=0

    let s:lightline_theme="seoul256"
    colorscheme miramare
elseif s:theme == 3
    let g:ayucolor="mirage"
    let s:lightline_theme="ayu_mirage"
    colorscheme ayu
endif

if s:theme == 2 || s:theme == 3
    set hlsearch
    hi clear Search
    hi Search gui=underline,bold

    set cursorline
    hi clear CursorLine
    hi CursorLineNR guifg=#607080

    hi CocRustTypeHint guifg=#607080
    hi CocRustChainingHint guifg=#959595
endif

let g:togglecursor_force = 'xterm'

match Error /\t/

" Status line
set laststatus=2
set noshowmode showcmd

" = gb color red
hi ExtraWhitespace guibg=#fb4934

function! DisplayGitBranch()
    let branch = fugitive#head()
    if len(branch) > 0
        return "\ue0a0 " . fugitive#head()
    endif
    return ""
endfunction

let g:lightline = {
      \ 'colorscheme': s:lightline_theme,
      \ 'active': {
      \   'left': [ ['mode', 'paste']
      \           , ['readonly', 'filename', 'modified']
      \           , ['gitbranch']
      \           ],
      \   'right': [ ['position']
      \            , ['fileencoding', 'filetype']
      \            , ['total']
      \            ],
      \ },
      \ 'component': {
      \   'position': '%l:%c',
      \   'total': '%L',
      \ },
      \ 'component_function': {
      \   'gitbranch': 'DisplayGitBranch',
      \ },
      \ }
""/ THEME

" Writing
let g:vimwiki_list = [ {
     \ 'path': '~/vimwiki/',
     \ 'syntax': 'markdown',
     \ 'ext': '.notes.md',
     \ } ]

let g:goyo_width  = '60%'
let g:goyo_height = '80%'

function! s:SetIndentation(size)
    let &shiftwidth=a:size
    let &tabstop=a:size
    let &softtabstop=a:size
endfunction
au BufRead,BufNew *.notes.md call s:SetIndentation(3)

" Languages

" indent
let g:haskell_indent_disable = 1

set colorcolumn=120
au FileType haskell,cabal set cc=80

au FileType svg,xml set nowrap
au FileType svg,xml call s:SetIndentation(2)

au FileType toml,json set cc=80
au FileType toml call s:SetIndentation(2)

" completion
let g:user_emmet_leader_key='<C-_>'
au FileType html.twig.js.css EmmetInstall

" fixes
au FileType html.twig.js.css silent call CocActionAsync('activeExtension', 'coc-html')

function! s:onCocInit()
    let &ft=&ft
endfunction
autocmd User CocNvimInit call s:onCocInit()

" QoL
let g:UltiSnipsEditSplit="vertical"

""" KEY BINDINGS
nnoremap <SPACE> <Nop>
let mapleader=" "

" v(im) shortcuts
" R(eload all)
nnoremap <leader>vR :source $HOME/.vimrc<CR>
" r(elaod): c(urrent), p(lugins)
nnoremap <leader>vrc :source %<CR>
nnoremap <leader>vrp :source $HOME/.vimrc<CR>:PlugUpdate<CR>

" f(ile) shortcuts
" s(ave)
nnoremap <leader>fs :w<CR>

nnoremap <silent> <leader>/ :GitFiles<CR>
nnoremap <silent> <leader>. :Files<CR>
nnoremap <silent><expr> <leader>, ':e#' . v:count . '<CR>'

nnoremap <silent> <leader>\ :TagbarToggle<CR>

" window/tab/file/buffer movements
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
set nosplitbelow splitright

" s(witch tab)
nnoremap <leader>sp :tabp<CR>
nnoremap <leader>sn :tabn<CR>
nnoremap <leader>sx :tabclose<CR>

" Writing
nnoremap <silent> <leader>W :Goyo<CR>
" mark task as (un)done
nnoremap <silent> <leader>wx F[dt]i[x<ESC>$
nnoremap <silent> <leader>wu F[dt]i[ <ESC>$

" QoL shortcuts
nnoremap <leader>d "_d
nnoremap <silent> <leader>P :set paste!<CR>

" git
nnoremap <silent> <leader>gs :G<CR>
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gg :GitGutterToggle<CR>

" transform bindings
nnoremap <silent> <leader>tW :StripWhitespace<CR>
nnoremap <silent> <leader>tT :retab<CR>

" COC - Most keybindings here are defaults
" from the README page of coc.nvim as I find them convenient
if has('nvim')
    inoremap <silent><expr> <c-space> coc#refresh()
else
    inoremap <silent><expr> <c-@> coc#refresh()
endif
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>"
inoremap <silent><expr> <C-P> CocActionAsync('showSignatureHelp')

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" g[dYir] -> goto code
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" refactoring
nmap <silent> <leader>rn <Plug>(coc-rename)

nmap <silent> <leader>A <Plug>(coc-codeaction)
nmap <silent> <leader>a <Plug>(coc-codeaction-selected)
" q(quick) f(ix)
nmap <silent> <leader>qf <Plug>(coc-fix-current)

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

