{
  description = "Ryan's NixOS configurations";

  nixConfig = {
    substituters =
      [ "https://ryantking.cachix.org" "https://nix-community.cachix.org" ];
    trusted-public-keys = [
      "ryantking.cachix.org-1:FQS/rxVvhhWSUds7Fcmf4RNdp95gVRHaDxorFuVIgXE="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin-stable.url = "github:nixos/nixpkgs/nixpkgs-21.05-darwin";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";

    rnix-lsp.url = "github:nix-community/rnix-lsp";

    treefmt.url = "github:numtide/treefmt";

    tmux-fzf = {
      url = "github:sainnhe/tmux-fzf";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, darwin, ... }:
    let
      modules = [
        ./modules/desktop
        ./modules/theme
        ./modules/shell
        ./modules/devel
        ./modules/editors
      ];

      overlays = [ inputs.neovim-nightly-overlay.overlay ] ++ map import [
        ./overlays/fennel.nix
        ./overlays/tmux-fzf.nix
        ./overlays/vim-plugins.nix
        ./overlays/yabai.nix
      ];

      mkNixosConfig = { baseModule, extraModules ? modules }:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            nixpkgs = inputs.nixpkgs;
          };
          modules = [
            baseModule
            inputs.home-manager.nixosModules.home-manager
            { nixpkgs.overlays = overlays; }
            ./modules/common
            ./modules/nixos
          ] ++ extraModules;
        };

      mkDarwinConfig = { baseModule, extraModules ? modules }:
        darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          specialArgs = {
            inherit inputs;
            nixpkgs = inputs.nixpkgs;
          };
          modules = [
            baseModule
            { nixpkgs.overlays = overlays; }
            inputs.home-manager.darwinModules.home-manager
            ./modules/common
            ./modules/darwin
          ] ++ extraModules;
        };

    in {
      nixosConfigurations = {
        trashstation = mkNixosConfig { baseModule = ./hosts/trashstation; };
      };

      darwinConfigurations = {
        trashbook = mkDarwinConfig { baseModule = ./hosts/trashbook.nix; };
      };
    };
}
