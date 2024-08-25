#!/usr/bin/env bash

bat=$(cat /sys/class/power_supply/BAT0/capacity)
status=$(cat /sys/class/power_supply/BAT0/status)

icon_charge=🔌
icon_ramp0=
icon_ramp25=
icon_ramp50=
icon_ramp75=
icon_ramp100=

# icon_ramp10=󰁺
# icon_ramp20=󰁻
# icon_ramp30=󰁼
# icon_ramp40=󰁽
# icon_ramp50=󰁾
# icon_ramp60=󰁿
# icon_ramp70=󰂀
# icon_ramp80=󰂁
# icon_ramp90=󰂂
# icon_ramp100=󰁹

if [[ "$status" == "Charging" ]]; then
    echo "$icon_charge $bat%"
elif [[ $bat -lt 5 ]]; then
    echo "$icon_ramp0 $bat%"
elif [[ $bat -le 30 ]]; then
    echo "$icon_ramp25 $bat%"
elif [[ $bat -le 60 ]]; then
    echo "$icon_ramp50 $bat%"
elif [[ $bat -lt 85 ]]; then
    echo "$icon_ramp75 $bat%"
else
    echo "$icon_ramp100 $bat%"
fi

# battery.sh ends here
