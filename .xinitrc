#!/bin/sh
###
# .xinitrc
##
# Used to start the X server with xmonad,
#  most configuration in this file would be useless
#  if used with a less configured WM
#
# Notes: Includes only settings that can be shared across devices,
#  machine-dependent settings are located in ".xinitrc-local"
###

xset r rate 600 35

[[ -f "$HOME/.Xmodmap" ]] && xmodmap "$HOME/.Xmodmap"
[[ -f "$HOME/.xbindkeysrc.scm" ]] && xbindkeys -f "$HOME/.xbindkeysrc.scm"
[[ -f "$HOME/.Xresources" ]] && xrdb -load "$HOME/.Xresources"

[[ -f "$HOME/.xinitrc-local" ]] && sh "$HOME/.xinitrc-local"
[[ -f "$HOME/.inputremap" ]] && sh "$HOME/.inputremap"

# XDG
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache

# Reset pipes
function mkpipe {
    if [[ -p "$1" ]]; then
        rm "$1"
    fi
    mkfifo "$1"
}

mkpipe "/tmp/xmonad-info"
mkpipe "/tmp/discord.pipe"

# Set wallpaper
[[ -f $HOME/.fehbg ]] && sh $HOME/.fehbg

# Variables used in xmonad config
export XMONAD=$HOME/.xmonad
export XMONAD_CONFIG_DIR=$XMONAD/bin/xmonad
export XMONAD_DATA_DIR=$XMONAD_CONFIG_DIR
export XMONAD_CACHE_DIR=$XDG_CACHE_HOME/xmonad

export XMOBAR_CONFIG_DIR=$XMONAD/bin/xmobar

# Start xmonad
exec qmonad
