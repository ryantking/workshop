{ config, lib, pkgs, options, inputs, ... }:

let
  inherit (builtins) hasAttr;
  inherit (config) theme colorscheme;
  inherit (lib) mkMerge optionals optionalAttrs;
  inherit (pkgs.stdenv) isDarwin;
  inherit (pkgs.lib.our) mkNerdFamily;
in
{
  programs.alacritty = mkMerge [
    {
      enable = true;
      settings = {
        env.TERM = "alacritty-direct";
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
          decorations = if isDarwin then "buttonless" else "full";
        };

        colors = (import ./colors.nix {
          inherit lib;
          inherit (colorscheme) colors;
        });

        font = with theme.codeFont; {
          inherit size;

          normal = {
            inherit style;
            family = "${mkNerdFamily family} Nerd Font Mono";
          };
        };
      };
    }
    (optionalAttrs isDarwin {
      settings.key_bindings = (import ./darwin-bindings.nix);
    })
  ];
}
