{ config, lib, pkgs, ... }:

let
  inherit (builtins) elem toString;
  inherit (lib) isBool filterAttrs;
  inherit (lib.generators) toKeyValue;
  inherit (config.home) cacheHome;

  mkArgString = toKeyValue {
    mkKeyValue = key: value:
      let
        value' =
          if isBool value then
            (if value then "on" else "off")
          else
            toString value;
      in
      "${key}='${value'}' \\";
  };

  mkRule = { app, ... }@args:
    let args' = (filterAttrs (n: _: !elem n [ "app" ]) args);
    in "yabai -m rule --add app='${app}' ${mkArgString args'}";
in
{
  imports = [ ./skhd.nix ./simple-bar.nix ];

  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;

    config = {
      layout = "bsp";
      window_placement = "second_child";
      top_padding = 48;
      bottom_padding = 18;
      left_padding = 18;
      right_padding = 18;
      window_gap = 18;
      window_shadow = "on";
      window_border = "off";
      window_topmost = "on";
    };

    extraConfig = ''
      yabai -m rule --add app='^System Information$' manage=off
      yabai -m rule --add app='^System Preferences$' manage=off
      yabai -m rule --add title='Preferences$' manage=off
      yabai -m rule --add title='Settings$' manage=off
      yabai -m rule --add title='SentinelOne$' manage=off
    '';
  };

  launchd.user.agents.yabai.serviceConfig = {
    StandardOutPath = "${cacheHome}/logs/yabai.out.log";
    StandardErrorPath = "${cacheHome}/logs/yabai.err.log";
  };
}
