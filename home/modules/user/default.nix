{ config, lib, pkgs, ... }:

with lib.types;

let
  inherit (config.home) homeDirectory;
  inherit (pkgs.stdenv) isDarwin;
  inherit (pkgs.lib.our) mkOpt;
in
{
  options = {
    whoami = {
      name = mkOpt str "Ryan King";
      timezone = mkOpt str "America/New_York";

      usernames = {
        github = mkOpt str "ryantking";
      };

      emails = {
        personal = mkOpt str "ryantking@protonmail.com";
        gmail = mkOpt str "ryan.taylor.king@gmail.com";
        work = mkOpt str "ryking@redhat.com";
      };

      keys = {
        pgp = mkOpt str "0xD718BA353C298BB2";
        ssk = {
          primary = mkOpt str (import ./ssh-key.nix);
          identities = mkOpt (listOf str) (import ./ssh-identities.nix);
        };
      };

      xdg = {
        bin = mkOpt path "${homeDirectory}/.local/bin";
        cache = mkOpt path "${homeDirectory}/.cache";
        config = mkOpt path "${homeDirectory}/.config";
        data = mkOpt path "${homeDirectory}/.local/share";
        lib = mkOpt path "${homeDirectory}/.local/lib";
        state = mkOpt path "${homeDirectory}/.local/state";
      };
    };

    workshop = {
      dir = mkOpt path "${homeDirectory}/Workshop";
      configDir = mkOpt path "${config.workshop.dir}/etc";
    };
  };
}
