#!/bin/sh

# PATH
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.cabal/bin:$HOME.ghcup/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.luarocks/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

# Fixes
export _JAVA_AWT_WM_NONREPARENTING=1
