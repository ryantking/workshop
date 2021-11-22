{ config, pkgs, lib, ... }:

{
  imports = [ ./alacritty.nix ./kitty.nix ];

  config = {
    environment.variables.TERMINAL = "alacritty";

    hm.fonts = lib.optionalAttrs pkgs.stdenv.isLinux { fontconfig.enable = true; };

    fonts = {
      fonts = with pkgs; [
        corefonts
        fira
        redhat-official-fonts
        roboto
        roboto-slab
        scientifica
        (nerdfonts.override { fonts = [ "FiraCode" "Monoid" ]; })
      ];
    } // (if pkgs.stdenv.isDarwin then {
      enableFontDir = true;
    } else {
      fontDir.enable = true;
      enableGhostscriptFonts = true;

      fontconfig = {
        enable = true;
        defaultFonts = with config.theme.fonts; {
          monospace = [ mono.family ];
          sansSerif = [ sans.family ];
          serif = [ serif.family ];
        };
      };
    });
  };
}
