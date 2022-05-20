{ lib
, suites
, modulesPath
, ...
}: {
  imports = suites.base ++ [ (modulesPath + "/profiles/qemu-guest.nix") ];

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  networking = {
    useDHCP = false;
    hostName = "trashstation";
    interfaces.enp1s0.useDHCP = true;
  };

  boot = {
    cleanTmpDir = true;
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "virtio_pci" "usbhid" "sr_mod" "virtio_blk" ];
      kernelModules = [ ];
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
      device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
    };

    "/boot" = {
      device = "/dev/disk/by-label/BOOT";
      fsType = "vfat";
    };
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  hardware.cpu.intel.updateMicrocode = true;

  programs.git = {
    enable = true;
    config.init.defaultBranch = "master";
  };
}
