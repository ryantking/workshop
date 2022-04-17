{ config, lib, pkgs, ... }:

with lib.types;

let
  inherit (lib) mkOption mkIf concatStringsSep mapAttrsToList;
  inherit (pkgs.lib.our) mkOpt';
  inherit (config.home) cacheHome;

  cfg = config.services.skhd;
in
{
  options.services.skhd = {
    logPath = mkOpt' path "${cacheHome}/logs/skhd.log" "Path to write logs to";

    bindings = mkOpt' attrs { } "Keybindings";

    extraConfig = mkOpt' str "" "Extra configuration";
  };

  config = mkIf cfg.enable {
    services.skhd.skhdConfig =
      let
        mkBinding = key: cmd: "${key} : ${cmd}";
      in
      ''
        ${concatStringsSep "\n" (mapAttrsToList mkBinding cfg.bindings)}

        ${cfg.extraConfig}
      '';

    launchd.user.agents.skhd.serviceConfig = {
      StandardOutPath = cfg.logPath;
      StandardErrorPath = cfg.logPath;
    };
  };
}
