{ inputs, pkgs, lib, ... }:

let
  inherit (lib) mkOption types;
  inherit (pkgs.stdenv) isDarwin;
in {
  imports = [ ./nixpkgs.nix ./options.nix ./home-manager.nix ];

  options = {
    workshopDir = mkOption {
      description = "Directory containing the workshop";
      default = "$HOME/Workshop";
      type = types.str;
    };
  };

  config.environment = {
    systemPackages = with pkgs;
      [ file git neovim pciutils rclone unzip wget ]
      ++ (if isDarwin then [ coreutils-prefixed ] else [ coreutils binutils ]);

    etc = {
      home-manager.source = "${inputs.home-manager}";
      nixpkgs.source = "${inputs.nixpkgs}";
    };
  };
}
