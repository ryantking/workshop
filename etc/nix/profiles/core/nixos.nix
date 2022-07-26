{
  self,
  config,
  lib,
  pkgs,
  ...
}: {
  nix = {
    autoOptimiseStore = true;
    nixPath = ["nixos-config=${../../lib/compat/nixos}"];
    optimise.automatic = true;
    systemFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
  };

  system.stateVersion = "22.05";

  security.sudo.wheelNeedsPassword = false;

  boot = {
    loader.systemd-boot.consoleMode = "auto";
    cleanTmpDir = lib.mkDefault true;
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  programs.git = {
    enable = true;
    config.init.defaultBranch = "master";
  };

  services = {
    earlyoom.enable = true;

    openssh = {
      enable = lib.mkDefault true;
      openFirewall = true;
      passwordAuthentication = false;
      permitRootLogin = "prohibit-password";
    };
  };

  environment = {
    shellAliases.nixos-option = "nixos-option -I nixpkgs=${self}/etc/nix/lib/compat";

    systemPackages = with pkgs; [
      dosfstools
      dnsutils
      efibootmgr
      gptfdisk
      inetutils
      iputils
      mtr
      pciutils
      sysstat
      usbutils
      utillinux
    ];
  };
}
