{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./fzf.nix ./starship.nix];

  programs = {
    zsh.envExtra = let
      vars = lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: ''export ${n}="${v}"'') config.shell.env);
    in ''
      has() {
        type "$1" >/dev/null 2>&1
      }

      ${vars}
    '';

    dircolors.enable = true;
  };

  home.file.".dir_colors".source = "${pkgs.sources.nord-dircolors.src}/src/dir_colors";

  shell = {
    env = with config.xdg; {
      PATH = ["$XDG_BIN_HOME" "$PATH"];
      CARGO_HOME = "$XDG_DATA_HOME/cargo";
      RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
    };

    abbrs = import ./abbrs.nix;
    aliases = import ./aliases.nix;
  };
}
