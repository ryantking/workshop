#!/usr/bin/env sh

sketchybar --add item calendar right \
    --set calendar update_freq=15 \
    icon=cal \
    icon.color=$FG \
    icon.padding_left=12 \
    label.align=right \
    label.color=$FG \
    label.padding_right=12 \
    align=center \
    script="$PLUGIN_DIR/time.sh"
