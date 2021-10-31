{ config, pkgs, lib, ... }:

{
  imports = [ ./alacritty.nix ];

  config = {
    environment.variables.TERMINAL = "alacritty";

    hm.fonts =
      lib.optionalAttrs pkgs.stdenvNoCC.isLinux { fontconfig.enable = true; };

    fonts = {
      enableFontDir = true;

      fonts = with pkgs; [
        corefonts
        fira
        redhat-official-fonts
        roboto
        roboto-slab
        scientifica
        (nerdfonts.override { fonts = [ "FiraCode" "Monoid" ]; })
      ];
    } // lib.optionalAttrs pkgs.stdenvNoCC.isLinux {
      enableGhostscriptFonts = true;

      fontconfig = {
        enable = true;
        defaultFonts = with config.theme.fonts; {
          monospace = [ mono.family ];
          sansSerif = [ sans.family ];
          serif = [ serif.family ];
        };
      };
    };
  };
}
