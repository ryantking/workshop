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

    kernelPackages = pkgs.linuxPackages_latest;

    consoleLogLevel = 3;

    kernel.sysctl = {
      "vm.swappiness" = 0;
      "kernel/sysrq" = 1;
    };
  };
}
