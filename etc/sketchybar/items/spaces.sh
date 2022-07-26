#!/usr/bin/env sh

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
	icon.highlight_color=$CYAN \
	label.drawing=off \
	click_script="yabai -m space --focus \$SID 2>/dev/null"
}

SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")

for i in "${!SPACE_ICONS[@]}"; do
    sid=$(($i+1))
    add_space $sid ${SPACE_ICONS[i]}
done

sketchybar \
    --add item separator left \
    --set separator \
    icon=$SEPARATOR \
    icon.font="FiraCode Nerd Font:Regular:16.0" \
    background.padding_left=12 \
    background.padding_right=12 \
    label.drawing=off \
    icon.color=$FG
    

