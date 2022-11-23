#!/usr/bin/env sh

POPUP_OFF="sketchybar --set apple.logo popup.drawing=off"
POPUP_CLICK="sketchybar --set \$NAME popup.drawing=toggle"

sketchybar \
    --add item apple.logo left \
    --set apple.logo \
    icon=$APPLE \
    icon.font="$FONT:Black:16.0" \
    icon.color=$FG \
    background.padding_left=5 \
    background.padding_right=10 \
    label.drawing=off \
    click_script="$POPUP_CLICK" \
    --add item apple.prefs popup.apple.logo \
    --set apple.prefs \
    icon=$PREFERENCES \
    label="Preferences" \
    click_script="open -a 'System Preferences'; $POPUP_OFF" \
    --add item apple.activity popup.apple.logo \
    --set apple.activity \
    icon=$ACTIVITY \
    label="Activity" \
    click_script="open -a 'Activity Monitor'; $POPUP_OFF" \
    --add item apple.lock popup.apple.logo \
    --set apple.lock \
    icon=$LOCK \
    label="Lock Screen" \
    click_script="pmset displaysleepnow; $POPUP_OFF"
