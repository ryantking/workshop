{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption types;

  cfg = config.rust;
in {
  options.rust = {
    rustupHome = mkOption {
      description = "Home directory for rust insalls";
      default = "$HOME/.local/share/rustup";
      type = types.str;
    };

    cargoHome = mkOption {
      description = "Home directory for Cargo packages";
      default = "$HOME/.local/share/cargo";
      type = types.str;
    };
  };

  config.hm.home = {
    packages = with pkgs; [ rustup rust-analyzer ];
    sessionVariables = {
      RUSTUP_HOME = cfg.rustupHome;
      CARGO_HOME = cfg.cargoHome;
    };
  };
}
