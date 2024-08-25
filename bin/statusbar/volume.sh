#!/usr/bin/env bash

mute=$(pamixer --get-mute)
vol=$(pamixer --get-volume)

icon_mute=󰝟
icon_ramp1=󰕿
icon_ramp2=󰖀
icon_ramp3=󰕾

if [[ "$mute" == "true" ]]; then
  echo "$icon_mute off"
elif [[ "$vol" -ge 100 ]]; then
  echo "$icon_ramp3 $vol%"
elif [[ "$vol" -ge 50 ]]; then
  echo "$icon_ramp2 $vol%"
elif [[ "$vol" -gt 0 ]]; then
  echo "$icon_ramp1 $vol%"
else
  echo "$icon_mute $vol%"
fi

# volume.sh ends here
