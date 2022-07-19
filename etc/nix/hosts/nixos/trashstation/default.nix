{
  config,
  lib,
  profiles,
  suites,
  modulesPath,
  pkgs,
  ...
}: let
  inherit (config.whoami.keys) ssh;
in {
  imports =
    [
      (modulesPath + "/profiles/qemu-guest.nix")
      ./hardware-configuration.nix
    ]
    ++ suites.desktop
    ++ (with profiles; [hardware.intel hardware.nvidia hardware.hidpi zfs river]);

  networking = {
    hostName = "trashstation";
    hostId = "8556b001";
    useDHCP = false;
    interfaces.enp7s0.useDHCP = true;
  };
}
