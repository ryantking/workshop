{
  description = "Ryan's NixOS configurations";

  outputs = inputs: import ./etc/nix inputs;

  inputs = {
    # Channels
    nixos.url = "github:nixos/nixpkgs/nixos-22.05";
    latest.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-22.05-darwin";
    nixpkgs.follows = "nixos";

    # Configuration Layers
    darwin = {
      url = "github:ryantking/nix-darwin";
      inputs.nixpkgs.follows = "nixos";
    };
    home = {
      url = "github:ryantking/home-manager/mbsync-launchd";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    # Flake Utilities
    digga = {
      url = "github:divnix/digga/home-manager-22.11";
      inputs = {
        nixpkgs.follows = "nixos";
        nixlib.follows = "nixos";
        darwin.follows = "darwin";
        home-manager.follows = "home";
        deploy.follows = "deploy";
        devshell.follows = "devshell";
      };
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
    nixos-generators.url = "github:nix-community/nixos-generators";

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
    ragenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixos";
    };

    # Development Tools
    emacs.url = "github:nix-community/emacs-overlay";
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
