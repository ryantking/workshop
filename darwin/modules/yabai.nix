{ config, lib, pkgs, ... }:

with lib.types;

let
  inherit (config.home) cacheHome;
  inherit (builtins) elem;
  inherit (lib) mkOption mkIf concatMapStringsSep concatImapStringsSep filterAttrs isBool;
  inherit (lib.generators) toKeyValue;
  inherit (pkgs.lib.our) mkOpt';

  cfg = config.services.yabai;
in
{
  options.services.yabai = {
    logPath = mkOpt' path "${cacheHome}/logs/yabai.log" "Path to write logs to";

    spaces = mkOpt' (listOf str) [ ] "Space labels";

    rules = mkOpt' (listOf attrs) [ ] "Rules for specific applications";

    signals = mkOpt' (listOf attrs) [ ] "Signal event definitions";
  };

  config = mkIf cfg.enable {
    services.yabai.extraConfig =
      let
        mkLabel = n: label: "yabai -m space ${toString n} --label '${label}'";

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
          let args' = filterAttrs (n: _: ! elem n [ "app" ]) args;
          in "yabai -m rule --add app='${app}' ${mkArgString args'}";

        mkSignal = { event, action, ... }@args:
          let args' = filterAttrs (n: _: ! builtins.elem n [ "event" "action" ]) args;
          in "yabai -m signal --add event='${event}' action='${action}' ${mkArgString args'}";
      in
      ''
        ${concatImapStringsSep "\n" mkLabel cfg.spaces}

        ${concatMapStringsSep "\n" mkRule cfg.rules}

        ${concatMapStringsSep "\n" mkSignal cfg.signals}
      '';

    launchd.user.agents.yabai.serviceConfig = {
      StandardOutPath = cfg.logPath;
      StandardErrorPath = cfg.logPath;
    };
  };
}
