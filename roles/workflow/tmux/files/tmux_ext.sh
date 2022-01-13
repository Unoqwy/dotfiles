#!/bin/sh
# vim:fdm=marker

LAYOUTS_DIR="$HOME/.local/share/tmux/layouts"
LAYOUTS_MAP="$HOME/.local/share/tmux/layouts-map"
SESSIONS_MAP="$HOME/.local/share/tmux/sessions-map"

LAYOUT_NONE=("none" "null" "nil")

## Layouts {{{
# $1: layout name
# >stdout: layout file
_tmuxext_layout_edit() {
    if [[ -z "$1" ]]; then
        return 1
    fi

    if [[ ! -d $LAYOUTS_DIR ]]; then
        mkdir -p "$LAYOUTS_DIR"
    fi
    file="$LAYOUTS_DIR/$1"
    if [[ ! -f $file ]]; then
        echo "#!/bin/sh" >> "$file"
    fi
    if [[ ! -x $file ]]; then
        chmod +x "$file"
    fi
    echo "$file"
}

# $1: layout name
# >stdout(?): layout file
# ret: whether file exists
_tmuxext_layout_get() {
    if [[ -z "$1" ]]; then
        return 1
    fi

    file="$LAYOUTS_DIR/$1"
    if [[ -f $file ]]; then
        echo "$file"
        return 0
    fi
    return 1
}

# $1: layout name
# ret: whether file was deleted
_tmuxext_layout_delete() {
    if [[ -z "$1" ]]; then
        return 1
    fi

    file="$LAYOUTS_DIR/$1"
    if [[ -f $file ]]; then
        rm "$file"
        return 0
    fi
    return 1
}

# $1: session name
_tmuxext_autoload() {
    session="$1"
    if [[ -z "$session" ]]; then
        return 1
    fi

    local -A map
    if [[ -f "$LAYOUTS_MAP" ]]; then
        source "$LAYOUTS_MAP"
    else
        return 1
    fi

    if [[ ! -v 'map[$1]' ]]; then
        return 1
    fi

    mapped=${map[$session]}
    layout=$(echo $mapped | cut -d, -f 1)
    cwd=$(echo $mapped | cut -d, -f 2)

    layout_file=$(_tmuxext_layout_get "$layout")
    if [[ -z "$layout_file" ]]; then
        return 1
    fi
    if [[ ! -x "$layout_file" ]]; then
        echo "Layout '$layout' is not stored properly (i.e. not executable)"
        return 1
    fi

    tmux new-session -d -s "$session" -c "$cwd"
    _tmuxext_apply "$layout_file" "$session" "$cwd"
    mktmux="$cwd/.mktmux"
    if [[ -x "$mktmux" ]]; then
        _tmuxext_apply "$layout_file" "$session"
    fi
}

# $1: layout file
# $2: session name
# $3: working directory
_tmuxext_apply() {
    if [[ ! -x "$1" ]]; then
        return 1
    fi

    session="$2"
    (
        _w() {
            tmux neww -d -t "$session" $@
        }

        cd "$3"
        source "$1" "$session"
    )
}

# $1: session name
# $2?: whether session should be autoloaded if not existing
# ret: whether session now exists
_tmuxext_ensure_session() {
    session="$1"
    if ! tmux list-sessions | cut -d':' -f 1 | grep -qx "$session"; then
        if [[ $2 != 0 ]]; then
            if ! _tmuxext_autoload "$session"; then
                return 1
            fi
        else
            return 1
        fi
    fi
    return 0
}
## }}}

## Ext. commands {{{
function ta() {
    if [[ ! $# -eq 1 ]]; then
        echo "Usage: ta <session name>"
        return 1
    fi

    session="$1"
    if ! _tmuxext_ensure_session "$session"; then
        echo "Unkown session"
        return 1
    fi

    if [[ -n "$TMUX" ]]; then
        tmux switch -t "$session"
    else
        tmux attach -t "$session"
    fi
}

function tt() {
    if [[ $# -gt 1 ]]; then
        echo "Usage: tt [session name] [set to layout]"
        return 1
    fi

    autoload=1
    if [[ -n $1 ]]; then
        if [[ $1 == .* ]]; then
            session="${1:1}"
            autoload=0 # overwrite previous sessions
        else
            ta "$1"
            return 0
        fi
    else
        local -A map
        if [[ -f "$SESSIONS_MAP" ]]; then
            source "$SESSIONS_MAP"
        fi

        if [[ -v 'map[$PWD]' ]]; then
            session=${map[$PWD]}
        else
            session=$(basename $(realpath .) | tr '.' '-')
            session=${session// /_}
        fi
    fi
    if ! _tmuxext_ensure_session "$session" $autoload; then
        tl map ${2:-default} "$session"
    fi
    ta "$session"
}

function tl() {
    layout="$2"
    if [[ -n "$layout" && $(echo "${LAYOUT_NONE[@]}" | grep -io "$layout") ]]; then
        layout="none"
    fi
    case $1 in
        edit)
            if [[ -z "$layout" ]]; then
                echo "Usage: tl edit <layout>"
                return 1
            elif [[ $layout == "none" ]]; then
                echo "This layout name is reserved."
                return 1
            fi

            file=$(_tmuxext_layout_edit "$layout")
            "$EDITOR" "$file"
            ;;
        alias)
            session="$layout"
            if [[ -z "$session" ]]; then
                echo "Usage: tl alias <session name>"
                return 1
            fi

            local -A map
            if [[ -f "$SESSIONS_MAP" ]]; then
                source "$SESSIONS_MAP"
            fi

            if [[ $session == "none" ]]; then
                unset "map[$PWD]"
            else
                map[$PWD]="$session"
            fi
            if declare -p map > "$SESSIONS_MAP"; then
                if [[ $session == "none" ]]; then
                    echo "Deleted mapping for current working directory."
                else
                    echo "Set mapping for current working directory to session '$session'."
                fi
            fi
            ;;
        load|map)
            if [[ -z "$layout" ]]; then
                echo "Usage: tl $1 <layout> [session name]"
                return 1
            fi

            if [[ $layout != "none" ]]; then
                layout_file=$(_tmuxext_layout_get "$layout")
                if [[ -z "$layout_file" ]]; then
                    echo "Layout does not exist"
                    return 1
                fi
            fi

            session="$3"
            if [[ -z "$session" && -n "$TMUX" ]]; then
                session=$(tmux display-message -p "#S")
            fi
            if [[ -n "$session" ]]; then
                case $1 in
                    load)
                        if [[ $layout == "none" ]]; then
                            echo "The null layout cannot be dynamically loaded."
                            return 1
                        fi
                        if [[ ! -x "$layout_file" ]]; then
                            echo "This layout cannot be loaded."
                            return 1
                        fi

                        if tmux list-sessions | cut -d':' -f 1 | grep -qx "$session"; then
                            tmux kill-session -t "$session"
                        fi
                        if _tmuxext_autoload "$session"; then
                            echo "Applied layout '$layout' on session '$session'."
                        else
                            echo "Error while applying layout '$layout' on session '$session'."
                            return 1
                        fi
                        ;;
                    map)
                        local -A map
                        if [[ -f "$LAYOUTS_MAP" ]]; then
                            source "$LAYOUTS_MAP"
                        fi
                        if [[ $layout == "none" ]]; then
                            unset "map[$session]"
                        else
                            map[$session]="$layout,$PWD"
                        fi
                        if declare -p map > "$LAYOUTS_MAP"; then
                            if [[ $layout == "none" ]]; then
                                echo "Deleted mapping for session name '$session'."
                            else
                                echo "Set mapping for session name '$session' to layout '$layout' with current directory."
                            fi
                        fi
                        ;;
                esac
            else
                echo "Session name cannot be infered because you are not within a tmux session. Please specify it manually."
                return 1
            fi
            ;;
        delete)
            if [[ -z "$layout" ]]; then
                echo "Usage: tl delete <layout>"
                return 1
            elif [[ $layout == "none" ]]; then
                echo "This layout name is reserved."
                return 1
            fi

            if _tmuxext_layout_delete "$layout"; then 
                echo "Layout '$layout' deleted"
            else
                echo "Layout '$layout' does not exist"
                return 1
            fi
            ;;
        *)
            echo "Usage: tl edit <layout> OR tl load <layout> [session name] OR tl map <layout> [session name] OR tl delete <layout>"
            return 1
    esac
}
## }}}

## Aliases {{{
# quickly set session working direcftory
alias th="tmux command-prompt -I \$PWD -p 'CWD:' 'attach -c %1'"
alias thc="th && clear"
alias tk="tmux kill-session -t"
## }}}
