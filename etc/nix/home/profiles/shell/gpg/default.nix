{ pkgs, ... }:

{
  home.packages = with pkgs; [ gpg-tui pinentry-curses ];

  programs.gpg = { enable = true; };

  services.gpg-agent = (optionalAttrs isLinux {
    enable = true;
    defaultCacheTtl = 300;
    pinentryFlavor = "curses";
    enableSshSupport = true;
    sshKeys = [ "74CB8154D23D820537432F4B73446CF35A14216F" ];
  });
}
