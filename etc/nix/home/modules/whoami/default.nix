{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  inherit (config.home) homeDirectory;
  inherit (pkgs.lib.our) mkOpt;
in {
  options.whoami = {
    name = mkOpt types.str "Ryan King";
    timezone = mkOpt types.str "America/New_York";

    usernames = {
      github = mkOpt types.str "ryantking";
    };

    emails = {
      personal = mkOpt types.str "ryantking@protonmail.com";
      gmail = mkOpt types.str "ryan.taylor.king@gmail.com";
      work = mkOpt types.str "ryking@redhat.com";
    };

    keys = {
      pgp = {
        primary = mkOpt types.str "0xD718BA353C298BB2";
        machine = mkOpt (types.nullOr types.str) null;
      };

      ssh = {
        primary = mkOpt types.str (import ./ssh-key.nix);
        identities = mkOpt (types.listOf types.str) (import ./ssh-identities.nix);
      };
    };

    xdg = {
      bin = mkOpt types.path "${homeDirectory}/.local/bin";
      cache = mkOpt types.path "${homeDirectory}/.cache";
      config = mkOpt types.path "${homeDirectory}/.config";
      data = mkOpt types.path "${homeDirectory}/.local/share";
      lib = mkOpt types.path "${homeDirectory}/.local/lib";
      state = mkOpt types.path "${homeDirectory}/.local/state";
    };
  };
}
