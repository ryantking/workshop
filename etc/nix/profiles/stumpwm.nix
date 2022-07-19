{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver = {
    desktopManager.plasma5.enable = false;
    windowManager.stumpwm.enable = true;
  };

  environment.systemPackages = [pkgs.stumpish];
}
