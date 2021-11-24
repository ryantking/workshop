{ pkgs, lib, ... }:

let
  inherit (lib) mkMerge mkIf optionalAttrs;
  inherit (pkgs.stdenv) isDarwin isLinux;
in {
  environment.shellInit = ''
    export GPG_TTY="$(tty)"
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    gpgconf --launch gpg-agent
  '';

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    ssh = optionalAttrs isLinux { startAgent = false; };
  };

  hm = {
    programs.gpg = { enable = true; };
    home.packages = with pkgs; [ gpg-tui pinentry-curses ];

    services.gpg-agent = (optionalAttrs isLinux {
      enable = true;
      defaultCacheTtl = 300;
      pinentryFlavor = "curses";
      enableSshSupport = true;
      sshKeys = [ "74CB8154D23D820537432F4B73446CF35A14216F" ];
    });
  };
}
