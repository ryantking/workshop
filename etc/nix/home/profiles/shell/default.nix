{ config, lib, ... }:

let
  inherit (lib) concatStringsSep mapAttrsToList;
in
{
  imports = [ ./fzf.nix ];

  environment.extraInit =
    let
      vars = concatStringsSep "\n" (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.my.env);
    in
    ''
      has() {
        type "$1" >/dev/null 2>&1
      }

      ${vars}
    '';

  my = {
    env = {
      PATH = [ "$XDG_BIN_HOME" "$PATH" ];
      CARGO_HOME = "$XDG_DATA_HOME/cargo";
      RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
    };

    shell = {
      abbrs = import ./abbrs.nix;
      aliases = import ./aliases.nix;
    };
  };
}
