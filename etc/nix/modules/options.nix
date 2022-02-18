{ config, options, lib, pkgs, ... }:

with lib.types;

let
  inherit (builtins) elem getEnv;
  inherit (lib) mkAliasDefinitions;
  inherit (pkgs.lib.our) mkOpt;

  username =
    let envName = getEnv "USER";
    in if elem envName [ "" "root" ] then "rking" else envName;
in
{
  imports = [
    (lib.mkAliasOptionModule [ "user" ] [ "home-manager" "users" username "user" ])
    (lib.mkAliasOptionModule [ "workshop" ] [ "home-manager" "users" username "workshop" ])
    (lib.mkAliasOptionModule [ "theme" ] [ "home-manager" "users" username "theme" ])
    (lib.mkAliasOptionModule [ "home" "configHome" ] [ "home-manager" "users" username "xdg" "configHome" ])
    (lib.mkAliasOptionModule [ "home" "dataHome" ] [ "home-manager" "users" username "xdg" "dataHome" ])
    (lib.mkAliasOptionModule [ "home" "cacheHome" ] [ "home-manager" "users" username "xdg" "cacheHome" ])
  ];

  options = {
    username = mkOpt str username;

    home = {
      file = mkOpt attrs { };
      configFile = mkOpt attrs { };
      dataFile = mkOpt attrs { };
    };
  };

  config.home-manager.users.${username} = {
    home.file = mkAliasDefinitions options.home.file;

    xdg = {
      configFile = mkAliasDefinitions options.home.configFile;
      dataFile = mkAliasDefinitions options.home.dataFile;
    };
  };
}
