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

# Handy aliases
alias .f="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

