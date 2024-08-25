#!/usr/bin/env bash

icon="󰓇"
status=$(playerctl -p spotifyd metadata --format '{{artist}} - {{title}}' | cut -c 1-50)
runstate=$(playerctl -p spotifyd status)

if [[ $runstate == "Playing" ]]; then
    echo "$icon $status"
else
    echo ""
fi

# spotify.sh ends here
