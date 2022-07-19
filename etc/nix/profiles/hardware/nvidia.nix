{
  config,
  lib,
  pkgs,
  ...
}: {
  nixpkgs.config.allowUnfree = true;

  hardware = {
    enableAllFirmware = true;
    enableRedistributableFirmware = true;

    nvidia.modesetting.enable = true;
  };

  boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

  services.xserver.videoDriver = "nvidia";
}
