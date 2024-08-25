#!/usr/bin/env bash

icon_wifi=
icon_eth=
icon_dc=❎
icon_vpn=🔒
icon_dnd=

net="$icon_dc "
vpn=""
dnd=""
if tailscale status >/dev/null 2>&1; then
  vpn="$icon_vpn"
fi

if [[ "$(dunstctl is-paused)" == true ]]; then
  dnd="$icon_dnd "
fi

if [[ "$(cat /sys/class/net/w*/operstate 2>/dev/null)" == "up" ]]; then
  net="$icon_wifi "
elif [[ "$(cat /sys/class/net/e*/operstate 2>/dev/null)" == "up" ]]; then
  net="$icon_eth"
fi

echo -e "$vpn $net $dnd"

# indicators.sh
