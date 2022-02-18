{ config, lib, ... }:

let
  inherit (lib) concatStringsSep mapAttrsToList;
in
{
  imports = [ ./fzf.nix ];

  programs.zsh.envExtra =
    let
      vars = concatStringsSep "\n" (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.shell.env);
    in
    ''
      has() {
        type "$1" >/dev/null 2>&1
      }

      ${vars}
    '';

  shell = {
    env = with config.xdg; {
      PATH = [ "$XDG_BIN_HOME" "$PATH" ];
      CARGO_HOME = "$XDG_DATA_HOME/cargo";
      RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
    };

    abbrs = import ./abbrs.nix;
    aliases = import ./aliases.nix;
  };
}
