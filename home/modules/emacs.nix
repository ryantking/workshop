{ config, lib, pkgs, ... }:

with lib.types;

let
  inherit (config.xdg) configHome dataHome;
  inherit (pkgs.lib.our) mkOpt;

  cfg = config.emacs;
in
{
  options.emacs = {
    configDir = mkOpt path "${configHome}/emacs";

    doom = {
      configDir = mkOpt path "${config.workshop.configDir}/doom";
      dataDir = mkOpt path "${dataHome}/doom";
    };
  };

  config.shell.env = {
    EMACSDIR = cfg.configDir;
    DOOMDIR = cfg.doom.configDir;
    DOOMLOCALDIR = cfg.doom.dataDir;
  };
}
