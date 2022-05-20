#!/usr/bin/env bash

PATH="/run/current-system/sw/bin:/etc/profiles/per-user/rking/bin:$PATH"
source $HOME/.config/sketchybar/colors.sh

update() {
  WINDOW=$(yabai -m query --windows --window)
  CURRENT=$(echo "$WINDOW" | jq '.["stack-index"]')

  args=()
  if [[ $CURRENT -gt 0 ]]; then
    LAST=$(yabai -m query --windows --window stack.last | jq '.["stack-index"]')
    args+=(--set $NAME icon=􀏭 label.drawing=on label=$(printf "[%s/%s]" "$CURRENT" "$LAST"))
    yabai -m config active_window_border_color $COLOR_BLUE

  else
    args+=(--set $NAME label.drawing=off)
    case "$(echo "$WINDOW" | jq '.["is-floating"]')" in
    "false")
      if [ "$(echo "$WINDOW" | jq '.["has-fullscreen-zoom"]')" = "true" ]; then
        args+=(--set $NAME icon="􀏜")
        yabai -m config active_window_border_color $COLOR_YELLOW
      elif [ "$(echo "$WINDOW" | jq '.["has-parent-zoom"]')" = "true" ]; then
        args+=(--set $NAME icon="􀥃")
        yabai -m config active_window_border_color $COLOR_RED
      else
        args+=(--set $NAME icon="􀏝")
        yabai -m config active_window_border_color $COLOR_CYAN
      fi
      ;;
    "true")
      args+=(--set $NAME icon="􀢌")
      yabai -m config active_window_border_color $COLOR_PURPLE
      ;;
    esac
  fi

  sketchybar -m "${args[@]}"
}

mouse_clicked() {
  yabai -m window --toggle float
  update
}

mouse_entered() {
  sketchybar --set $NAME background.drawing=on
}

mouse_exited() {
  sketchybar --set $NAME background.drawing=off
}

case "$SENDER" in
"mouse.entered")
  mouse_entered
  ;;
"mouse.exited")
  mouse_exited
  ;;
"mouse.clicked")
  mouse_clicked
  ;;
"forced")
  exit 0
  ;;
*)
  update
  ;;
esac
