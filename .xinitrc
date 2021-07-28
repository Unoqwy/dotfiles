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
xmodmap "$HOME/.Xmodmap"

if [[ -f "$HOME/.xinitrc-local" ]]; then
    sh "$HOME/.xinitrc-local"
fi

# Variables used in xmonad config
export XDG_CONFIG_HOME=$HOME/.config
export XMONAD=$HOME/.xmonad

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
sh $HOME/.fehbg

# Start xmonad
export XMONAD_CONFIG_DIR=$XMONAD/bin/xmonad
export XMOBAR_CONFIG_DIR=$XMONAD/bin/xmobar
export XMONAD_DATA_DIR=$XMONAD_CONFIG_DIR
exec qmonad

