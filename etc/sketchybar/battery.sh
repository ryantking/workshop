#!/usr/bin/env dash

PATH="/run/current-system/sw/bin:/etc/profiles/per-user/rking/bin:$PATH"

DETAILS="$(pmset -g batt)"
SOURCE="$(echo "$DETAILS" | grep -o "\'.*\'" | sed "s/'//g")"
PERCENT="$(echo "$DETAILS" | grep -o "\d*\%")"
REMAINING="$(echo "$DETAILS" | grep -o " \d*:\d* ")"

sketchybar --set $NAME icon="$PERCENT" label="$REMAINING"
