{
  self,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  inherit (config.home) homeDirectory;
  inherit (config.workshop) configHome;
  inherit (pkgs.lib.our) mkOpt;

  hostName = "shoyobook"; # TODO: un-hardcode
  hosts = builtins.fromTOML (builtins.readFile "${self}/etc/hosts.toml");
  hostCfg = hosts.${hostName};
  peerKeys = lib.filterAttrs (n: _: n != hostName);
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
      age = mkOpt types.str hostCfg.keys.age;

      pgp = {
        primary = mkOpt types.str "0xD718BA353C298BB2";
        machine = mkOpt types.str hostCfg.keys.pgp;
      };

      ssh = {
        primary = mkOpt types.str (import ./ssh-key.nix);
        identities = mkOpt (types.listOf types.str) hostCfg.keys.ssh;
      };

      peers = {
        age = mkOpt (types.listOf types.str) (builtins.mapAttrs (_: v: v.age) peerKeys);
        pgp = mkOpt (types.listOf types.str) (builtins.mapAttrs (_: v: v.pgp) peerKeys);
        ssh = mkOpt (types.listOf types.str) (builtins.concatLists (builtins.mapAttrs (_: v: v.ssh) peerKeys));
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
