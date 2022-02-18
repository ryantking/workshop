{ config, lib, pkgs, ... }:

let
  inherit (config.theme) codeFont;
  inherit (lib) mkMerge optionalAttrs;
  inherit (pkgs.stdenv) isLinux;
in
{
  fonts = (mkMerge [
    {
      enableFontDir = true;
      fonts = with pkgs; [ corefonts ibm-plex overpass ];
    }
    (optionalAttrs isLinux {
      fontconfig.defaultFonts = {
        serif = [ "IBM Plex Mono" ];
        sansSerif = [ "Overpass" ];
        monospace = [ "Victor Mono" ];
      };
    })
  ]);
}
