{ inputs, pkgs, ... }:

{
  user.name = "rking";

  # services.nix-daemon.enable = true;
  # nix.useDaemon = true;

  theme = {
    colorscheme = "nord";

    fonts = rec {
      mono = {
        family = "Monoid Nerd Font";
        style = "Retina";
        size = 12;
        pkg = pkgs.nerdfonts.override;
      };
    };
  };
}
