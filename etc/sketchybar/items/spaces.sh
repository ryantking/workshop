#!/usr/bin/env bash

function add_space {
    SID=$1
    ICON=$2

    sketchybar \
        --add space space.$SID left \
        --set space.$SID \
        associated_space=$SID \
        icon=$ICON \
        icon.padding_left=8 \
        icon.padding_right=8 \
        background.padding_left=5 \
        background.padding_right=5 \
        background.corner_radius=5 \
        background.height=22 \
        background.drawing=off \
        background.color=$ACTIVE_SPACE_COLOR \
        label.drawing=off \
        script="$PLUGIN_DIR/spaces.sh" \
        click_script="yabai -m space --focus $SID 2>/dev/null"
}

SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")

for i in "${!SPACE_ICONS[@]}"; do
    sid=$(($i + 1))
    add_space $sid ${SPACE_ICONS[i]}
done
