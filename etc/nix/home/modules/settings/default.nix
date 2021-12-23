{ config, pkgs, lib, home-manager, options, ... }:

with lib.types;

let
  inherit (lib) mkOption mapAttrs isList concatMapStringSep;
  inherit (pkgs.lib.our) mkOpt mkOpt' mkBoolOpt;
  inherit (config.settings.user) home;

  sshDir = "${home}/.ssh";
in {
  options.settings = {
    name = mkOpt str "Ryan King";
    timezone = mkOpt str "America/New_York";
    githubUsername = mkOpt str "ryantking";
    email = mkOpt str "ryantking@protonmail.com";
    terminal = mkOpt str "alacritty";
    user = mkOption { type = options.users.users.type.functor.wrapped; };

    keys = {
      pgp = mkOpt str "0xD718BA353C298BB2";
      ssh = mkOpt str (import ./ssk-key.nix);
    };

    hm = mkOption {
      description = "Primary user's home-manager configuration";
      type = attrs;
      default = { };
    };

    xdg = {
      bin = mkOpt path "${home}/.local/bin";
      cache = mkOpt path "${home}/.cache";
      config = mkOpt path "${home}/.config";
      data = mkOpt path "${home}/.local/share";
      lib = mkOpt path "${home}/.local/lib";
      state = mkOpt path "${home}/.local/state";
    };

    env = mkOption {
      description = "Environment variables.";
      type = attrsOf (oneOf [ str path (listOf (either str patch)) ]);
      apply = mapAttrs
        (n: v: if isList v then concatMapStringSep ":" (x: toString x) v else (toString v));
    };
  };
}
