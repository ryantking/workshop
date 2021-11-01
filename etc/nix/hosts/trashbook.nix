{ inputs, pkgs, ... }:

{
  user = {
    description = "Ryan King";
    name = "rking";
    home = "/Users/rking";
  };

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
