#!/bin/bash

PLUGINS_DIR="$HOME/.local/share/tmux/plugins"
mkdir -p "$PLUGINS_DIR" &>/dev/null

function use {
    local_repo="$PLUGINS_DIR/$1"
    if [[ ! -d "$local_repo" ]]; then
        git clone "$2" "$local_repo"
    fi

    if [[ -z "$TMUX_ONLY_DL" ]]; then
        sh "$local_repo/$3"
    fi
}

use tmux-resurrect "https://github.com/tmux-plugins/tmux-resurrect" "resurrect.tmux"
