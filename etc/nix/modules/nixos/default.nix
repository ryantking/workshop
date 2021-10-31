{ config, pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  config = {
    system.stateVersion = "21.05";
    time.timeZone = "America/New_York";
    i18n.defaultLocale = "en_US.UTF-8";

    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    networking = {
      useDHCP = false;
      networkmanager = {
        enable = true;
        packages = [ pkgs.networkmanager-openvpn ];
      };
    };

    programs.git = {
      enable = true;
      config.init.defaultBranch = "master";
    };

    security.sudo.wheelNeedsPassword = false;
    services.xserver = {
      enable = true;
      layout = "us";
      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;
    };

    environment.systemPackages = with pkgs; [ psmisc usbutils ];

    hm.home.packages = with pkgs; [
      copyq
      brave
      discord
      font-manager
      iotop
      protonmail-bridge
      signal-desktop
      slack
      tdesktop
      vlc
      zoom-us
    ];
  };
}
