#!/bin/sh

bindkey -v
setopt autocd

# Completion
autoload -Uz compinit; compinit
setopt MENU_COMPLETE
zstyle ":completion:*" menu select

bindkey '^[[Z' reverse-menu-complete

# Prompt
autoload -Uz promptinit; promptinit

if [[ "$TERM" != "dumb" ]]; then
    autoload -Uz vcs_info
    zstyle ":vcs_info:*" enable git
    precmd() { vcs_info; }

    zstyle ":vcs_info:git*" formats "  %b" # TODO: display more git info

    setopt PROMPT_SUBST
    # reminder: these need to stay single quotes to expand later, not on read
    PS1='%F{red}%(?.. <%?>)%F{magenta}${vcs_info_msg_0_} %b%F{green}%f '
    RPS1="%F{blue}%~%f"
else
    # based on default prompt 'walters'
    PS1="%(?..[%?] )%n@%m:%~ "
fi


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
alias inhist="history 0 | grep -i"

# Syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

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
alias lh="ls -lah"
alias yank="xclip -selection clipboard"

# git
alias g="git"
alias gc="git commit"
alias gs="git status"
alias gcl="git clone"
alias lg="lazygit"

function gclh { git clone "https://github.com/$1" ${@:2} }
function gclb { git clone "https://gitlab.com/$1.git" ${@:2} }

# tmux
alias ta="tmux attach -t"
alias th="tmux command-prompt -I \$PWD -p 'CWD:' 'attach -c %1'"
alias thc="th && clear"
alias tml="tmux ls"

if type nvim > /dev/null; then
    alias vim="nvim"
    alias vi="nvim"
fi

alias ssh="TERM=xterm-256color ssh"

# misc tools
alias ts="tree-sitter"
alias f="xplr"

# system
alias synclock="sudo ntpd -gq"

# cargo run test with stdout
alias cto="cargo test -- --nocapture"

# Symfony
alias sf="symfony"
alias sfs="(pkill symfony || true) && symfony server:start"
alias sfc="php bin/console"
