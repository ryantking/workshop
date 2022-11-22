{
  config,
  lib,
  profiles,
  suites,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ] ++ suites.desktop ++ (with profiles; [kde]);

  boot = {
    initrd = {
      availableKernelModules = ["xhci_pci" "ahci" "virtio_pci" "sr_mod" "virtio_blk"];
      kernelModules = ["dm-snapshop"];
    };

    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/483e3e4f-b43b-45a5-985b-b87bdca947c3";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/31A7-80D3";
      fsType = "vfat";
    };
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/9b333c9a-9c9b-4dc4-bc16-a3f3ac1b08ba";}
  ];

  networking.hostName = "triangle-vm";
}
