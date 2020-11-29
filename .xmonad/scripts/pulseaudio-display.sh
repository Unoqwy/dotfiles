#!/bin/sh
icon=$(if [[ "$(pamixer --get-mute)" == "true" ]]; then echo "\uf6a9"; else echo "\uf58f"; fi)
echo -e "<fn=2>$icon</fn><fn=1> </fn>$(pamixer --get-volume)%"
