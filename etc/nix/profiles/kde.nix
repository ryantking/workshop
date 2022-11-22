{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver = {
    enable = true;
    desktopManager.plasma5.enable = lib.mkDefault true;

    displayManager = {
      sddm.enable = true;

      autoLogin = {
        enable = true;
        user = "ryan";
      };
    };
  };

  xdg.portal = {
    enable = config.services.xserver.desktopManager.plasma5.enable;
    gtkUsePortal = true;
  };

  programs.gnupg.agent.pinentryFlavor = "qt";

  environment.systemPackages = with pkgs; [nordic];
}
