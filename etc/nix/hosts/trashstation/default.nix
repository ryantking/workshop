{ inputs, config, pkgs, ... }:

{
  require = [ ./hardware-configuration.nix ];

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        editor = false;
      };

      efi.canTouchEfiVariables = true;
    };

    cleanTmpDir = true;
  };

  networking = {
    hostName = "trashstation";
    interfaces.enp1s0.useDHCP = true;
  };

  users = {
    mutableUsers = false;
    users.root.hashedPassword =
      "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
  };

  user = {
    name = "rking";
    uid = 1000;
    description = "Ryan King";
    home = "/home/rking";
    isNormalUser = true;
    useDefaultShell = true;
    extraGroups = [ "wheel" "networkmanager" ];
    hashedPassword =
      "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
  };

  services.tailscale.enable = true;

  cpuVendor = "intel";
  gpuVendor = "nvidia";

  theme = {
    colorscheme = "nord";

    fonts = rec {
      sans = {
        family = "Roboto";
        style = "Light";
        size = 12;
        pkg = pkgs.roboto;
      };
      serif = {
        family = "Roboto Slab";
        style = "Light";
        size = 12;
        pkg = pkgs.roboto-slab;
      };
      mono = {
        family = "Fira Code Nerd Font";
        style = "Light";
        size = 12;
        pkg = pkgs.nerdfonts.override;
      };
      ui = sans;
    };
  };
}
