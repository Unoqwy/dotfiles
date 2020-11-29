call plug#begin('~/.vim/plugged-config')
Plug 'edkolev/tmuxline.vim'
call plug#end()

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

command TmuxBar Tmuxline lightline_visual
command TmuxBarPersist TmuxlineSnapshot! $XDG_CONFIG_HOME/tmux/statusline.conf

