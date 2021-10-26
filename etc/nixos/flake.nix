{
  description = "Ryan's NixOS configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";

    rnix-lsp.url = "github:nix-community/rnix-lsp";

    treefmt.url = "github:numtide/treefmt";

    tmux-fzf = {
      url = "github:sainnhe/tmux-fzf";
      flake = false;
    };
  };

  nixConfig = {
    substituters =
      [ "https://cache.nixos.org" "https://nix-community.cachix.org/" ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      inherit (lib) nixosSystem;
      inherit (lib.internal.files) mapFiles mapFilesRec mapFilesRecToList;

      supportedSystems = [ "x86_64-linux" ];

      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });

      lib =
        nixpkgs.lib.extend (final: prev: { internal = import ./lib final; });

      mkNixosConfig = path:
        nixosSystem {
          system = import (path + "/system.nix");
          specialArgs = { inherit inputs lib; };
          modules = [
            {
              nixpkgs.overlays = [] # [ inputs.neovim-nightly-overlay.overlay ]
                ++ self.overlays;
            }
            (import path)
          ] ++ mapFilesRecToList import ./modules;
        };

    in {
      lib = lib.internal;

      nixosConfigurations = mapFiles mkNixosConfig ./hosts;

      nixosModules = mapFilesRec import ./modules;

      overlays = mapFilesRecToList import ./overlays;
    };
}
