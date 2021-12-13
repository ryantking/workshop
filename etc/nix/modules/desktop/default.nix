{ config, pkgs, lib, ... }:

{
  imports = [ ./alacritty.nix ./kitty.nix ];

  config = let
    inherit (lib) mkMerge optionalAttrs;
    inherit (pkgs.stdenv) isDarwin isLinux;
  in {
    environment.variables.TERMINAL = "alacritty";

    hm.fonts = optionalAttrs isLinux { fontconfig.enable = true; };

    fonts = with pkgs;
      (mkMerge [
        {
          fonts = [
            corefonts
            emacs-all-the-icons-fonts
            redhat-official-fonts
            scientifica
            victor-mono
            (nerdfonts.override { fonts = [ "VictorMono" ]; })
          ];
        }
        (optionalAttrs isDarwin { enableFontDir = true; })
        (optionalAttrs isLinux {
          fontDir.enable = true;
          enableGhostscriptFonts = true;

          fonts = [ roboto roboto-slab ];

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
