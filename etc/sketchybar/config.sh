#!/usr/bin/env sh

CONFIG_DIR="$WORKSHOP_DIR/etc/sketchybar"
ITEM_DIR="$CONFIG_DIR/items"
PLUGIN_DIR="$CONFIG_DIR/plugins"

FONT="SF Pro"
PADDING=3

source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/icons.sh"

sketchybar --bar \
    height=32 \
    corner_radius=0 \
    border_width=0 \
    margin=0 \
    blur_radius=6 \
    position=top \
    padding_left=10 \
    padding_right=10 \
    color=$BAR_COLOR \
    topmost=off \
    font_smoothing=off \
    y_offset=0 \
    shadow=on \
    --default \
    drawing=on \
    update=when_shown \
    label.font="$FONT:Semibold:13.0" \
    icon.font="$FONT:Bold:14.0" \
    icon.color=$FG \
    label.color=$FG \
    icon.padding_left=$PADDING \
    icon.padding_right=$PADDING \
    label.padding_left=$PADDING \
    label.padding_right=$PADDING \
    background.padding_left=$PADDING \
    background.padding_right=$PADDING \
    popup.background.border_width=2 \
    popup.background.corner_radius=11 \
    popup.background.border_color=0x00000000 \
    popup.background.color=$BAR_COLOR \
    popup.background.shadow.drawing=on

source "$ITEM_DIR/apple.sh"
source "$ITEM_DIR/spaces.sh"
source "$ITEM_DIR/front_app.sh"

source "$ITEM_DIR/calendar.sh"
source "$ITEM_DIR/power.sh"
source "$ITEM_DIR/wifi.sh"
source "$ITEM_DIR/meetingbar.sh"

sketchybar --update

echo "sketchybar configuration loaded"
