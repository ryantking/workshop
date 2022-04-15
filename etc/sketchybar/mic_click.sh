#!/usr/bin/env bash

PATH="/run/current-system/sw/bin:/etc/profiles/per-user/rking/bin:$PATH"

MIC_VOLUME=$(osascript -e 'input volume of (get volume settings)')

if [[ $MIC_VOLUME -eq 0 ]]; then
    osascript -e 'set volume input volume 75'
    sketchybar --set $NAME icon=􀊱
elif [[ $MIC_VOLUME -gt 0 ]]; then
    osascript -e 'set volume input volume 0'
    sketchybar --set $NAME icon=􀊳
fi
