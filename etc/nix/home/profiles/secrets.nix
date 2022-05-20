{ config
, pkgs
, ...
}:

{
  age.identityPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];

  programs.password-store = {
    enable = true;
    settings = let inherit (config.whoami.keys) pgp; in {
      PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
      PASSWORD_STORE_KEY = "${pgp.primary} ${pgp.machine}";
    };
  };
}
