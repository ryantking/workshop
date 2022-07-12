{pkgs, ...}: {
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
}
