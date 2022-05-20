{ config
, lib
, pkgs
, ...
}: {
  nix.nixPath = [ "nixos-config=${../../lib/compat/nixos}" ];

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
      openFirewall = lib.mkDefault false;
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
    };
  };
}
