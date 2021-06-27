#!/bin/sh

bindkey -v
setopt autocd

# Completion
autoload -Uz compinit; compinit

# Prompt
autoload -Uz promptinit; promptinit
prompt walters
# TOOO: custom prompt, but I'll stay with this minimal prompt for a little while (only caveat no git status)

# Syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# History
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=$HISTSIZE

setopt HIST_IGNORE_ALL_DUPS

autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^k" up-line-or-beginning-search
bindkey "^j" down-line-or-beginning-search

alias hist="history"
alias inhist="history | grep -i"

# prevent accidental overwrite
alias cp="cp --interactive"
alias mv="mv --interactive"
alias rm="rm --interactive"

# colors
alias ls="ls --color=auto"
alias grep="grep --color=auto"

# common operations
alias cl="clear"
alias la="ls -a"
alias ll="ls -lh"
alias yank="xclip -selection clipboard"

# git
alias g="git"
alias gc="git commit"
alias gs="git status"

# tmux
alias ta="tmux attach -t"
alias th="tmux command-prompt -I \$PWD -p 'CWD:' 'attach -c %1'"
alias thc="th && clear"

# cargo run test with stdout
alias cto="cargo test -- --nocapture"

# Symfony
alias sf="symfony"
alias sfs="(pkill symfony || true) && symfony server:start"
alias sfc="php bin/console"
