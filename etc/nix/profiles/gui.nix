{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    firefox-devedition-bin
    discord
    element-desktop
    signal-desktop
    slack
    tdesktop
    _1password-gui
    vlc
    spotify
  ];

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      enableBrowserSocket = true;
    };

    mtr.enable = true;
  };
}
