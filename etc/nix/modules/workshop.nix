{ config, lib, pkgs, ... }:

let
  inherit (pkgs.lib.our) mkOpt;
  dir = (toString ../.);
in
{
  options = {
    workshop = with lib.types; {
      dir = mkOpt path dir;

      binDir = mkOpt path "${dir}/bin";
      configDir = mkOpt path "${dir}/..";
      libDir = mkOpt path "${dir}/lib";
      modulesDir = mkOpt path "${dir}/modules";
      profilesDir = mkOpt path "${dir}/profiles";
      secretsDir = mkOpt path "${dir}/secrets";
      vendorDir = mkOpt path "${dir}/vendor";

      path = mkOpt path "${config.my.user.home}/Workshop";
    };
  };
}
