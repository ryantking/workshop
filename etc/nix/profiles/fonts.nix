{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
in {
  environment.systemPackages = with pkgs; [
    (lib.mkIf isLinux font-manager)
  ];

  fonts = {
    fontDir.enable = lib.mkDefault true;

    fonts = with pkgs;
      [
        inter
      ]
      ++ (lib.optionals isLinux [
        corefonts
        inconsolata
        liberation_ttf
        dejavu_fonts
        bakoma_ttf
        gentium
        ubuntu_font_family
        terminus_font
      ]);
  };

  # environment.variables = lib.optionalAttrs pkgs.stdenv.isDarwin {
  #   FONTCONFIG_PATH = "/usr/local/etc/fonts";
  # };
  #
  # fonts = lib.mkMerge [
  #   {fonts = with pkgs; [corefonts ibm-plex];}
  #   (lib.optionalAttrs pkgs.stdenv.isLinux {
  #     fontDir.enable = true;
  #
  #     fontconfig.defaultFonts = {
  #       serif = ["IBM Plex Serif"];
  #       sansSerif = ["IBM Plex Sans"];
  #       monospace = ["RobotoMono Nerd Font"];
  #     };
  #   })
  # ];
}
