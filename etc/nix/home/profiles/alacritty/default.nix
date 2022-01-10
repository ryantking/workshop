{ config, lib, pkgs, options, inputs, ... }:

let
  inherit (builtins) hasAttr;
  inherit (config) theme colorscheme;
  inherit (lib) mkMerge optionals optionalAttrs;
  inherit (pkgs.stdenv) isDarwin;
in
mkMerge [
  {
    my.hm = {
      programs.alacritty = {
        enable = true;
        settings = {
          env.TERM = "alacritty";
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
              style = "Regular";
              family = "${nerdFamily} Nerd Font Mono";
            };
          };

          key_bindings = (optionals (isDarwin) (import ./darwin-bindings.nix));
        };
      };

      home.activation = (optionalAttrs (isDarwin) {
        "ensure-terminfo" = ''
          [[ -d "$HOME/.terminfo" ]] && exit 0

          echo "Installing additional terminal descriptions..."
          cd $(mktemp -d)
          curl -sLO https://invisible-island.net/datafiles/current/terminfo.src.gz
          gunzip terminfo.src.gz
          tic -xe alacritty,alacritty-direct,tmux-256color terminfo.src
        '';
      });
    };
  }

  (optionalAttrs (hasAttr "homebrew" options) {
    homebrew.casks = [ "alacritty" ];
  })
]
