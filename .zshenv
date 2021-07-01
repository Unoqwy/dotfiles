#!/bin/sh

export EDITOR=nvim

export XDG_CONFIG_HOME="$HOME/.config"

export GPG_TTY=$(tty)

# PATH
export PATH="$HOME/.local/bin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.cabal/bin:$HOME.ghcup/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.luarocks/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

# Fixes
export _JAVA_AWT_WM_NONREPARENTING=1
