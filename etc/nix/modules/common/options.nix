{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (options) home;
  inherit (lib) types;
  inherit (pkgs.lib.our) mkOpt;

  splitPath = lib.splitString ".";
  hmPathPrefix = ["home-manager" "users" username];
  mkHomeAlias = path: hmPath:
    lib.mkAliasOptionModule (splitPath path) (hmPathPrefix ++ (splitPath hmPath));

  username = let
    envName = builtins.getEnv "USER";
  in
    if builtins.elem envName ["" "root"]
    then "ryan"
    else envName;
in {
  imports = lib.mapAttrsToList mkHomeAlias {
    "whoami" = "whoami";
    "workshop" = "workshop";
    "theme" = "theme";
    "colorscheme" = "colorscheme";
    "shell" = "shell";
    "home.homeDirectory" = "home.homeDirectory";
    "home.configHome" = "xdg.configHome";
    "home.dataHome" = "xdg.dataHome";
    "home.cacheHome" = "xdg.cacheHome";
  };

  options = {
    username = mkOpt types.str username;

    home = {
      file = mkOpt types.attrs {};
      configFile = mkOpt types.attrs {};
      dataFile = mkOpt types.attrs {};
    };
  };

  config.home-manager.users.${username} = {
    home.file = lib.mkAliasDefinitions home.file;

    xdg = {
      configFile = lib.mkAliasDefinitions home.configFile;
      dataFile = lib.mkAliasDefinitions home.dataFile;
    };
  };
}
