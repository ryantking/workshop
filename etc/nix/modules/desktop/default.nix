{ config, pkgs, lib, ... }:

{
  imports = [ ./alacritty.nix ./kitty.nix ];

  config = let
    inherit (lib) mkMerge optionalAttrs;
    inherit (pkgs.stdenv) isDarwin isLinux;
    inherit (config.theme) fonts;
  in {
    environment.variables.TERMINAL = "alacritty";

    hm.fonts = optionalAttrs isLinux { fontconfig.enable = true; };

    fonts = with pkgs;
      (mkMerge [
        {
          fonts = [
            alegreya
            alegreya-sans
            corefonts
            emacs-all-the-icons-fonts
            merriweather
            redhat-official-fonts

            fonts.sans.pkg
            fonts.serif.pkg
            fonts.mono.pkg
            fonts.mono.nerdfont.pkg
            fonts.unicode.pkg
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
