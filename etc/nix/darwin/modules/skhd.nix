{ config
, lib
, pkgs
, ...
}:
let
  inherit (lib) types;
  inherit (pkgs.lib.our) mkOpt';

  cfg = config.services.skhd;
in
{
  options.services.skhd = {
    logPath = mkOpt' types.path "${config.home.cacheHome}/logs/skhd.log" "Path to write logs to";

    bindings = mkOpt' types.attrs { } "Keybindings";

    extraConfig = mkOpt' types.str "" "Extra configuration";
  };

  config = lib.mkIf cfg.enable {
    services.skhd.skhdConfig =
      let
        mkBinding = key: cmd: "${key} : ${cmd}";
      in
      ''
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkBinding cfg.bindings)}

        ${cfg.extraConfig}
      '';

    launchd.user.agents.skhd.serviceConfig = {
      StandardOutPath = cfg.logPath;
      StandardErrorPath = cfg.logPath;
    };
  };
}
