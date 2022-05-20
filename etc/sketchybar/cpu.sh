#!/usr/bin/env bash

PATH="/run/current-system/sw/bin:/etc/profiles/per-user/rking/bin:$PATH"
source $HOME/.config/sketchybar/colors.sh

CORE_COUNT=$(sysctl -n machdep.cpu.thread_count)
CPU_INFO=$(ps -eo pcpu,user)
CPU_SYS=$(echo "$CPU_INFO" | grep -v $(whoami) | sed "s/[^ 0-9\.]//g" | awk "{sum+=\$1} END {print sum/(100.0 * $CORE_COUNT)}")
CPU_USER=$(echo "$CPU_INFO" | grep $(whoami) | sed "s/[^ 0-9\.]//g" | awk "{sum+=\$1} END {print sum/(100.0 * $CORE_COUNT)}")

CPU_PERCENT="$(echo "$CPU_SYS $CPU_USER" | awk '{printf "%.0f\n", ($1 + $2)*100}')"

COLOR=$COLOR_GREEN
case "$CPU_PERCENT" in
[1-2][0-9])
  COLOR=$COLOR_YELLOW
  ;;
[3-6][0-9])
  COLOR=$COLOR_ORANGE
  ;;
[7-9][0-9] | 100)
  COLOR=$COLOR_RED
  ;;
esac

sketchybar --set cpu.percent label=$CPU_PERCENT% \
  label.color=$COLOR \
  --push cpu.sys $CPU_SYS \
  --push cpu.user $CPU_USER
