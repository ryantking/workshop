{ inputs, pkgs, ... }:

{
  user.name = "rking";

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
