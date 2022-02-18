{ config, lib, pkgs, ... }:

with lib.types;

let
  inherit (pkgs.stdenv) isDarwin;
  inherit (pkgs.lib.our) mkOpt;
in
{
  options = {
    user = {
      name = mkOpt str "Ryan King";
      email = mkOpt str "ryantking@protonmail.com";
      terminal = mkOpt str "alacritty";
      editor = mkOpt str "emacsclient";
      timezone = mkOpt str "America/New_York";
      githubUsername = mkOpt str "ryantking";

      keys = {
        pgp = mkOpt str "0xD718BA353C298BB2";
        ssh = mkOpt str (import ./ssh-key.nix);
      };

      xdg = let inherit (config.home) homeDirectory; in
        {
          bin = mkOpt path "${homeDirectory}/.local/bin";
          cache = mkOpt path "${homeDirectory}/.cache";
          config = mkOpt path "${homeDirectory}/.config";
          data = mkOpt path "${homeDirectory}/.local/share";
          lib = mkOpt path "${homeDirectory}/.local/lib";
          state = mkOpt path "${homeDirectory}/.local/state";
        };
    };

    workshop = {
      dir = mkOpt path "${config.home.homeDirectory}/Workshop";
      etcDir = mkOpt path "${config.workshop.dir}/etc";
    };
  };
}
