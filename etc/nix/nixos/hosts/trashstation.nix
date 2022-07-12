{
  lib,
  suites,
  modulesPath,
  ...
}: {
  imports = suites.base ++ suites.gui ++ suites.devel ++ [(modulesPath + "/profiles/qemu-guest.nix")];

  whoami.keys.pgp.machine = "0x7B9DDE8739045EF3"; # TODO: Replace with actual one

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  networking = {
    hostName = "trashstation";
    useDHCP = false;
    interfaces.enp7s0.useDHCP = true;
  };

  boot = {
    cleanTmpDir = true;
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];

    initrd = {
      availableKernelModules = ["xhci_pci" "ahci" "virtio_pci" "usbhid" "sr_mod" "virtio_blk"];
      kernelModules = [];
    };

    loader = {
      systemd-boot = {
        enable = true;
        editor = false;
      };

      efi.canTouchEfiVariables = true;
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/c9d75e76-ddff-4998-ba20-247b8b186164";
      fsType = "btrfs";
      options = ["subvol=root" "compress=zstd"];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/7E59-9436";
      fsType = "vfat";
    };

    "/home" = {
      device = "/dev/disk/by-uuid/c9d75e76-ddff-4998-ba20-247b8b186164";
      fsType = "btrfs";
      options = ["subvol=home" "compress=zstd"];
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/c9d75e76-ddff-4998-ba20-247b8b186164";
      fsType = "btrfs";
      options = ["subvol=nix" "compress=zstd" "noatime"];
    };

    "/swap" = {
      device = "/dev/disk/by-uuid/c9d75e76-ddff-4998-ba20-247b8b186164";
      fsType = "btrfs";
      options = ["subvol=swap" "noatime"];
    };
  };

  swapDevices = [{device = "/swap/swapfile";}];

  hardware.cpu.intel.updateMicrocode = true;

  programs.git = {
    enable = true;
    config.init.defaultBranch = "master";
  };
}
