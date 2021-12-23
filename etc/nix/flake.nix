{
  description = "Ryan's NixOS configurations";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters =
      "https://nrdxp.cachix.org https://ryantking.cachix.org https://nix-community.cachix.org";
    extra-trusted-public-keys =
      "nrdxp.cachix.org-1:Fc5PSqY2Jm1TrWfm88l6cvGWwz3s93c6IOifQWnhNW4= ryantking.cachix.org-1:FQS/rxVvhhWSUds7Fcmf4RNdp95gVRHaDxorFuVIgXE= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
  };

  inputs = {
    nixos.url = "github:nixos/nixpkgs/release-21.11";
    latest.url = "github:nixos/nixpkgs/nixos-unstable";

    darwin = {
      url = "github:ryantking/nix-darwin";
      inputs.nixpkgs.follows = "latest";
    };

    home = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixos";
    };

    nixos-hardware.url = "github:nixos/nixos-hardware";

    digga = {
      url = "github:divnix/digga";
      inputs = {
        nixpkgs.follows = "nixos";
        nixlib.follows = "nixos";
        home-manager.follows = "home";
      };
    };

    bud = {
      url = "github:divnix/bud";
      inputs = {
        nixpkgs.follows = "nixos";
        devshell.follows = "digga/devshell";
      };
    };

    nur.url = "github:nix-community/NUR";

    deploy = {
      url = "github:input-output-hk/deploy-rs";
      inputs.nixpkgs.follows = "nixos";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "latest";
    };

    nvfetcher = {
      url = "github:berberman/nvfetcher";
      inputs.nixpkgs.follows = "latest";
    };

    naersk = {
      url = "github:nmattia/naersk";
      inputs.nixpkgs.follows = "nixos";
    };

    nix-colors.url = "github:misterio77/nix-colors";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    doom-emacs-source = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    rnix-lsp.url = "github:nix-community/rnix-lsp";
    treefmt.url = "github:numtide/treefmt";
    tmux-fzf = {
      url = "github:sainnhe/tmux-fzf";
      flake = false;
    };
  };

  outputs = inputs:
    with inputs;
    with builtins;
    digga.lib.mkFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];

      channelsConfig = { allowUnfree = true; };

      channels = import ./channels { inherit self inputs; };

      lib = import ./lib { lib = digga.lib // nixos.lib; };

      sharedOverlays = import ./overlays/share { inherit self inputs; };

      nixos = ./nixos;

      home = ./home;

      devshell = ./shell;

      homeConfigurations = digga.lib.mkHomeConfigurations self.nixosConfigurations;
    };
}
