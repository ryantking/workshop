{ config
, lib
, pkgs
, ...
}: {
  programs.alacritty = lib.mkMerge [
    {
      enable = true;
      settings = {
        env = {
          TERM = "alacritty-direct";
          TERM_ANSI = "alacritty";
        };
        mouse.hide_when_typing = true;
        live_config_reload = true;
        use_thin_strokes = true;

        window = {
          dimension.columns = 120;
          padding = {
            x = 20;
            y = 20;
          };

          dynamic_title = true;
          decorations =
            if pkgs.stdenv.isDarwin
            then "buttonless"
            else "full";
        };

        colors = import ./colors.nix {
          inherit lib;
          inherit (config.colorscheme) colors;
        };

        font =
          let
            font = config.fonts.monospace;
          in
          {
            inherit (font) size;

            normal = {
              inherit (font) style;
              family = "${builtins.replaceStrings [" "] [""] font.family} Nerd Font";
            };
          };
      };
    }
    (lib.optionalAttrs pkgs.stdenv.isDarwin {
      settings.key_bindings = import ./darwin-bindings.nix;
    })
  ];
}
