HYPEN_INSENSITIVE="true"
ZSH_THEME="sorin"
COMPLETION_WAITING_DOTS="true"

plugins=(
    git
    vi-mode
)

# Load oh-my-zsh
# FIXME: get rid of oh-my-zsh or handle it better
DISABLE_AUTO_UPDATE="true"
source $HOME/.oh-my-zsh/oh-my-zsh.sh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Defaults & Compatibility
export EDITOR="nvim"
export XDG_CONFIG_HOME=$HOME/.config

# PATH
export PATH="$HOME/.cargo/bin:$PATH" # rust env
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH" # haskell env

# FIXME: quit using a bare repo to manage dotfiles
# default dotfiles path
export DOTFILES_GIT="$HOME/.dotfiles/"
export DOTFILES_HOME="$HOME"

# Handy aliases
alias .f="$(which git) --git-dir=$DOTFILES_GIT --work-tree=$DOTFILES_HOME"
alias .g="$(which lazygit) --git-dir=$DOTFILES_GIT --work-tree=$DOTFILES_HOME"
alias yank="xclip -selection clipboard"

# tmux aliases
# [a]ttach; cd [h]ere;
alias ta="tmux attach -t"
alias th="tmux command-prompt -I \$PWD -p 'CWD:' 'attach -c %1'"
