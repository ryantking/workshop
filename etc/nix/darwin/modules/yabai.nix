{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  inherit (lib.generators) toKeyValue;
  inherit (pkgs.lib.our) mkOpt';

  cfg = config.services.yabai;
in {
  options.services.yabai = {
    logPath = mkOpt' types.path "${config.home.cacheHome}/logs/yabai.log" "Path to write logs to";

    spaces = mkOpt' (types.listOf types.str) [] "Space labels";

    rules = mkOpt' (types.listOf types.attrs) [] "Rules for specific applications";

    signals = mkOpt' (types.listOf types.attrs) [] "Signal event definitions";
  };

  config = lib.mkIf cfg.enable {
    services.yabai.extraConfig = let
      mkLabel = n: label: "yabai -m space ${toString n} --label '${label}'";

      mkArgString = toKeyValue {
        mkKeyValue = key: value: let
          value' =
            if lib.isBool value
            then
              (
                if value
                then "on"
                else "off"
              )
            else toString value;
        in "${key}='${value'}' \\";
      };

      mkRule = {app, ...} @ args: let
        args' = lib.filterAttrs (n: _: ! builtins.elem n ["app"]) args;
      in "yabai -m rule --add app='${app}' ${mkArgString args'}";

      mkSignal = {
        event,
        action,
        ...
      } @ args: let
        args' = lib.filterAttrs (n: _: ! builtins.elem n ["event" "action"]) args;
      in "yabai -m signal --add event='${event}' action='${action}' ${mkArgString args'}";
    in ''
      ${lib.concatImapStringsSep "\n" mkLabel cfg.spaces}

      ${lib.concatMapStringsSep "\n" mkRule cfg.rules}

      ${lib.concatMapStringsSep "\n" mkSignal cfg.signals}
    '';

    launchd.user.agents.yabai.serviceConfig = {
      StandardOutPath = cfg.logPath;
      StandardErrorPath = cfg.logPath;
    };
  };
}
