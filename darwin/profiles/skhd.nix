{ lib, pkgs, ... }:

let
  inherit (lib) concatStringsSep mapAttrsToList mapAttrs';

  mkKey = mods: key: "${concatStringsSep " + " mods} - ${key}";

  numKeys = n: if n <= 0 then { } else let n' = toString n; in { "${n'}" = "${n'}"; } // numKeys (n - 1);

  commonKeys = {
    "x" = "recent";
    "z" = "prev";
    "c" = "next";
  };

  dirKeys = {
    "left" = "west";
    "right" = "east";
    "up" = "north";
    "down" = "south";
  };

  spaceKeys = (numKeys 9) // { "0" = "10"; } // commonKeys;

  displayKeys = (numKeys 2) // commonKeys;

  mkBindings = mods: mkCmd: keys:
    mapAttrs' (key: value: { name = "${mkKey mods key}"; value = "${mkCmd value}"; }) keys;

  mkFocusSpace = space: "yabai -m space --focus ${space}";

  mkFocusWindow = window: "yabai -m window --focus ${window}";

  mkFocusDisplay = display: "yabai -m display --focus ${display}";

  mkFocusWindowOrDisplay = dir: "${mkFocusWindow dir} || (${mkFocusDisplay dir})";

  mkMoveWindowSpace = space: "yabai -m window --space ${space}; ${mkFocusSpace space}";

  mkMoveWindowDisplay = display: "yabai -m window --display ${display}; ${mkFocusDisplay display}";

  mkSwapWindow = dir: "yabai -m window --swap ${dir} || (${mkMoveWindowDisplay dir})";
in
{
  services.skhd = {
    enable = true;
    package = pkgs.skhd;

    bindings = {
      "cmd + shift - return" = "open -na /Applications/Alacritty.app";
      "hyper - e" = "emacs";
      "cmd + alt - n" = ''yabai -m space --create && \
            index="(yabai -m query --spaces --display | jq 'map(select(.\"native-fullscreen\" == 0))[-1].index')" && \
            yabai -m space --focus "''${index}'';
      "cmd + alt - w" = "yabai -m space --destroy";
      "hyper - t" = ''yabai -m window --toggle float; yabai -m window --grid 4:4:1:1:2:2'';
    }
    // (mkBindings [ "cmd" "alt" ] mkFocusWindowOrDisplay dirKeys)
    // (mkBindings [ "cmd" "alt" "shift" ] mkSwapWindow dirKeys)
    // (mkBindings [ "cmd" "alt" ] mkFocusSpace spaceKeys)
    // (mkBindings [ "cmd" "alt" "shift" ] mkMoveWindowSpace spaceKeys)
    // (mkBindings [ "cmd" "ctrl" ] mkFocusDisplay displayKeys)
    // (mkBindings [ "cmd" "ctrl" "shift" ] mkMoveWindowDisplay displayKeys);
  };
}
