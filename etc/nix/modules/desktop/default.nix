{ config, pkgs, lib, ... }:

{
  imports = [ ./alacritty.nix ./kitty.nix ];

  config = let
    inherit (lib) mkMerge optionalAttrs;
    inherit (pkgs.stdenv) isDarwin isLinux;
  in {
    environment.variables.TERMINAL = "alacritty";

    hm.fonts = optionalAttrs isLinux { fontconfig.enable = true; };

    fonts = (mkMerge [
      {
        fonts = with pkgs; [
          corefonts
          emacs-all-the-icons-fonts
          fira
          redhat-official-fonts
          roboto
          roboto-slab
          scientifica
          (nerdfonts.override { fonts = [ "FiraCode" "Monoid" ]; })
        ];
      }
      (optionalAttrs isDarwin { enableFontDir = true; })
      (optionalAttrs isLinux {
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
      })
    ]);
  };
}
