#!/bin/sh
if [[ -n "TMUX_DL_PLUGINS" ]]; then
    exit 0
fi

PLUGINS_DIR="$HOME/.local/share/tmux/plugins"
mkdir -p "$PLUGINS_DIR" &>/dev/null

function use {
    local_repo="$PLUGINS_DIR/$1"
    if [[ ! -d "$local_repo" ]]; then
        git clone "$2" "$local_repo"
    fi

    sh "$local_repo/$3"
}

use tmux-resurrect "https://github.com/tmux-plugins/tmux-resurrect" "resurrect.tmux"
