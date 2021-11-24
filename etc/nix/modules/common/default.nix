{ inputs, pkgs, ... }:

let inherit (pkgs.stdenv) isDarwin;
in {
  imports = [ ./nixpkgs.nix ./options.nix ./home-manager.nix ];

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
