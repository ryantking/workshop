#!/usr/bin/env bash

PATH="/run/current-system/sw/bin:/etc/profiles/per-user/rking/bin:$PATH"
COUNT=0
COUNT=$(curl --max-time 20 https://github.com/users/ryantking/contributions | grep $(date '+%Y-%m-%d') | sed -nr 's/.*data-count=\"([^"]+).*/\1/p')

if [ "$COUNT" -gt "0" ]; then
    sketchybar --set $NAME icon.highlight=on label="$COUNT"
else
    if [ "$COUNT" = "" ]; then
        sketchybar --set $NAME icon.highlight=off label="!"
    else
        sketchybar --set $NAME icon.highlight=off label="$COUNT"
    fi
fi
