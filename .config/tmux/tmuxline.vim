" # How to use
" vim this file within a tmux session then source it
" use the command `:TmuxPersist` to perstist changes (warning: it overrides
" current settings)

call plug#begin('~/.vim/plugged-config')
" use a fork of tmuxline supporting lightline true colors
Plug 'hoov/tmuxline.vim/', { 'branch': 'truecolor-lightline' }
call plug#end()
:PlugInstall
:q

if !exists('*lightline#palette')
    throw "tmuxline: Can't load theme from lightline, function lightline#palette() doesn't exist. Is latest lightline loaded?"
endif

" tmuxline config
let g:tmuxline_powerline_separators = 0
let g:tmuxline_separators = {
    \ 'left': '',
    \ 'left_alt': '',
    \ 'right': '',
    \ 'right_alt': '',
    \ 'space': ' ',
    \ }

let g:tmuxline_preset = {
      \ 'a': '#S',
      \ 'y': '#H',
      \ 'z': '%I:%M %p',
      \ 'win': ["#I#F #W"],
      \ 'cwin': ["#I #W"],
      \ 'options': {
      \   'status-justify': 'left',
      \ },
      \ }

" set theme & persist command
let palette = lightline#palette()
let palette_for_visual = extend(deepcopy(palette.normal), palette.replace)
execute tmuxline#api#set_theme(tmuxline#util#create_theme_from_lightline(palette_for_visual))

command TmuxPersist TmuxlineSnapshot! $XDG_CONFIG_HOME/tmux/statusline.conf

