#!/usr/bin/env sh

CONFIG_DIR="$WORKSHOP_DIR/etc/sketchybar"
ITEM_DIR="$CONFIG_DIR/items"
PLUGIN_DIR="$CONFIG_DIR/plugins"

FONT="SF Pro"
PADDING=4

source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/icons.sh"

sketchybar --bar \
    height=42 \
    blur_radius=100 \
    position=top \
    padding_left=10 \
    padding_right=10 \
    color=$BAR_COLOR \
    shadow=on \
    --default \
    drawing=on \
    update=when_shown \
    label.font="$FONT:Light:14.0" \
    icon.font="$FONT:Regular:14.0" \
    icon.color=$FG \
    label.color=$FG \
    icon.padding_left=$PADDING \
    icon.padding_right=$PADDING \
    label.padding_left=$PADDING \
    label.padding_right=$PADDING

source "$ITEM_DIR/apple.sh"
source "$ITEM_DIR/spaces.sh"
source "$ITEM_DIR/front_app.sh"

source "$ITEM_DIR/calendar.sh"
source "$ITEM_DIR/power.sh"
source "$ITEM_DIR/wifi.sh"
source "$ITEM_DIR/meetingbar.sh"

sketchybar --update

echo "sketchybar configuration loaded"
