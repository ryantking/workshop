{ config, lib, pkgs, ... }:

let
  inherit (config.theme) codeFont;
  inherit (lib) mkMerge optionalAttrs;
  inherit (pkgs.stdenv) isDarwin isLinux;
in
{
  environment.variables = (optionalAttrs isDarwin {
    FONTCONFIG_PATH = "/usr/local/etc/fonts";
  });

  fonts = (mkMerge [
    {
      fonts = with pkgs; [ corefonts ibm-plex overpass ];
    }
    (optionalAttrs isLinux {
      enableFontDir = true;

      fontconfig.defaultFonts = {
        serif = [ "IBM Plex Mono" ];
        sansSerif = [ "Overpass" ];
        monospace = [ "Victor Mono" ];
      };
    })
  ]);
}
