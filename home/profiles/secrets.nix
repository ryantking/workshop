{ config, pkgs, ... }:

{
  age.identityPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts:
      with exts; [
        pass-import
        pass-otp
        pass-update
      ]);
    settings = {
      PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
      PASSWORD_STORE_KEY = "${config.whoami.keys.pgp} 0x7B9DDE8739045EF3";
    };
  };
}
