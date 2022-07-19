{
  config,
  lib,
  pkgs,
  ...
}: {
  xdg.portal = lib.mkIf config.xdg.portal.enable {
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };

  programs.dconf.enable = true;

  services.dbus = {
    enable = true;
    packages = [pkgs.dconf];
  };
}
