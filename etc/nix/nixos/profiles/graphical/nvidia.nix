{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    modesetting = {enable = true;};
  };

  systemd.services.nvidia-control-devices.wantedBy = ["multi-user.target"];

  boot.blacklistedKernelModules = ["nouveau"];
}
