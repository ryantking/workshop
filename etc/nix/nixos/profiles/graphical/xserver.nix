{
  pkgs,
  lib,
  ...
}: {
  hardware = {
    video.hidpi = {enable = true;};

    opengl = {
      enable = true;
      driSupport = true;
    };
  };

  environment.systemPackages = with pkgs; [
    adapta-gtk-theme
    gsettings-desktop-schemas
    imagemagick
    manpages
    papirus-icon-theme
    pulsemixer
    qt5.qtgraphicaleffects
    stdmanpages
    xsel
    zathura
    xdotool
    xorg.xwininfo
    obs-studio
  ];

  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager = {
      sddm = {enable = true;};

      autoLogin = {
        enable = true;
        user = "rking";
      };
    };
  };
}
