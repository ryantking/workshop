{ config
, lib
, pkgs
, ...
}:
let
  inherit (lib) types;
  inherit (pkgs.lib.our) mkOpt;

  workshopDir = "${config.home.homeDirectory}/Workshop";
in
{
  options.workshop = {
    home = mkOpt types.path workshopDir;
    configHome = mkOpt types.path "${workshopDir}/etc";
    dataHome = mkOpt types.path "${workshopDir}/usr/share";
  };
}
