{ config, lib, pkgs, ... }:

{
  i18n.defaultLocale = "en_US.UTF-8";

  environment.systemPackages = with pkgs; [
    coreutils
    dosfstools
    gptfdisk
    iputils
    usbutils
    utillinux
  ];

  services = {
    earlyoom = { enable = true; };

    tailscale = { enable = true; };

    openssh = {
      enable = true;
      passwordAuthentication = true;
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
    };
  };
}
