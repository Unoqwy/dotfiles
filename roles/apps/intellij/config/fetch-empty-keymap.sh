#!/bin/sh
set -e
base_keymaps="https://raw.githubusercontent.com/JetBrains/intellij-community/master/platform/platform-resources/src/keymaps"
out_dir=".cache/keymaps"
[[ ! -f "$out_dir" ]] && mkdir -p "$out_dir"

curl -s "$base_keymaps/%24default.xml" -o "$out_dir/default.xml"
curl -s "$base_keymaps/Default%20for%20XWin.xml" -o "$out_dir/xwin.xml"
(xpath -e '//action/@id' "$out_dir/default.xml" 2>&1 & xpath -e '//action/@id' "$out_dir/xwin.xml" 2>&1) \
    | grep -oP '(?<=id=")(.+)(?=")' \
    | sort | uniq \
    | while read id; do
        echo "<action id=\"$id\" />"
    done
