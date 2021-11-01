{ ... }:

{
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  hm = {
    home.stateVersion = "21.05";
    programs.home-manager.enable = true;
    systemd.user.startServices = true;
    xdg.enable = true;
  };
}
