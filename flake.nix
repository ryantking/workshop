{
  description = "Ryan's NixOS configurations";

  outputs = inputs: import ./etc/nix inputs;

  inputs = {
    # Channels
    nixos.url = "github:nixos/nixpkgs/nixos-21.11";
    latest.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";
    nixpkgs.follows = "nixos";

    # Configuration Layers
    darwin = {
      url = "github:ryantking/nix-darwin";
      inputs.nixpkgs.follows = "nixos";
    };
    home = {
      url = "github:ryantking/home-manager/mbsync-launchd";
      inputs.nixpkgs.follows = "nixos";
    };

    # Flake Utilities
    digga = {
      url = "github:divnix/digga";
      inputs = {
        nixpkgs.follows = "nixos";
        nixlib.follows = "nixos";
        darwin.follows = "darwin";
        home-manager.follows = "home";
        deploy.follows = "deploy";
        devshell.follows = "devshell";
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    deploy = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixos";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixos";
    };
    colors.url = "github:misterio77/nix-colors";

    # Source Management
    nvfetcher = {
      url = "github:berberman/nvfetcher";
      inputs.nixpkgs.follows = "nixos";
    };
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixos";
    };

    # Secrets Management
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixos";
    };
    agenix-cli = {
      url = "github:cole-h/agenix-cli";
      inputs.nixpkgs.follows = "nixos";
    };

    # Development Tools
    # emacs.url = "github:nix-community/emacs-overlay";
    treefmt.url = "github:numtide/treefmt";
    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs = {
        nixpkgs.follows = "nixos";
        naersk.follows = "naersk";
      };
    };
  };
}
