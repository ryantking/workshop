{
  config,
  lib,
  pkgs,
  ...
}: {
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    supportedFilesystems = ["zfs"];
    zfs.devNodes = "/dev/";
  };

  services.zfs = {
    trim.enable = true;
    autoScrub = {
      enable = true;
      pools = ["rpool"];
    };
  };
}
