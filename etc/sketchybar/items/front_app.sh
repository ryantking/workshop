#!/usr/bin/env sh

sketchybar \
    --add item front_app left \
    --set front_app \
    script="$PLUGIN_DIR/front_app.sh" \
    icon.drawing=off \
    background.padding_left=0 \
    label.color=$FG \
    label.font="$FONT:Bold:16.0" \
    --subscribe front_app front_app_switched
