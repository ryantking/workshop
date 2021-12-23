{ config, lib, pkgs, ... }:

let
  inherit (lib) mkMerge optionalAttrs;
  inherit (pkgs.stdenv) isLinux;
in {
  fonts = (mkMerge [
    {
      fontDir.enable = true;

      fonts = with pkgs; [ corefonts ibm-plex overpass victor-mono fira-mono ];
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
