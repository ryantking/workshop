{
  lib,
  pkgs,
  ...
}: {
  environment.variables = lib.optionalAttrs pkgs.stdenv.isDarwin {
    FONTCONFIG_PATH = "/usr/local/etc/fonts";
  };

  fonts = lib.mkMerge [
    {fonts = with pkgs; [corefonts ibm-plex];}
    (lib.optionalAttrs pkgs.stdenv.isLinux {
      enableFontDir = true;

      fontconfig.defaultFonts = {
        serif = ["IBM Plex Serif"];
        sansSerif = ["IBM Plex Sans"];
        monospace = ["RobotoMono Nerd Font"];
      };
    })
  ];
}
