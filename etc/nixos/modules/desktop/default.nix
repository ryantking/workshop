{ config, lib, inputs, pkgs, ... }:

let
  cfg = config.modules.desktop;
in {
  services.xserver.layout = "us";

  hm.home.packages = with pkgs; [
    discord
    slack
    gnome.seahorse
    gnome.gnome-font-viewer
    tdesktop
    vlc
  ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      fira
      roboto
      roboto-slab
      scientifica
      (nerdfonts.override { fonts = [ "FiraCode" "RobotoMono" ]; })
    ];

    fontconfig = {
      enable = true;
      defaultFonts = with config.modules.theme.fonts; {
        monospace = [ mono.family ];
        sansSerif = [ sans.family ];
        serif = [ serif.family ];
      };
    };
  };

  hm.fonts.fontconfig.enable = true;
}

