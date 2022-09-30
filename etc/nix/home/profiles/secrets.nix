{
  config,
  pkgs,
  ...
}: let
  inherit (config.whoami.keys) pgp;
in {
  age.identityPaths = ["${config.home.homeDirectory}/.ssh/id_ed25519"];

  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "${config.workshop.stateHome}/pass";
      PASSWORD_STORE_KEY = "${pgp.machine}";
    };
  };
}
