{
  description = "Ryan's NixOS configurations";

  inputs = {
    # Package Sets
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-latest.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";
    # nixpkgs.follows = "nixpkgs";

    # Flake Management
    digga = {
      url = "github:ryantking/digga";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixlib.follows = "nixpkgs";
        home-manager.follows = "home";
      };
    };
    darwin.follows = "digga/darwin";

    # System Management
    # darwin = {
    #   url = "github:ryantking/nix-darwin";
    #   inputs.nixpkgs.follows = "nixpkgs-unstable";
    # };
    # home = {
    #   url = "github:nix-community/home-manager/release-21.11";
    #   inputs.nixpkgs.follows = "nixpkgs-unstable";
    # };

    # Flake Utilities
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    colors.url = "github:misterio77/nix-colors";

    # Source Management
    nur.url = "github:nix-community/NUR";
    nvfetcher.url = "github:berberman/nvfetcher";

    # Secrets Management
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix-cli.url = "github:cole-h/agenix-cli";

    # Development Tools
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    # emacs.url = "github:nix-community/emacs-overlay";
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

  outputs = { self, nixpkgs, nixpkgs-latest, home, digga, utils, colors, agenix, nur, nvfetcher, ... } @ inputs:
    let
      inherit (nixpkgs.lib) mkMerge;
      inherit (digga.lib) mkFlake rakeLeaves importExportableModules mkHomeConfigurations;

      common = {
        modules = importExportableModules ./modules;

        profiles = rakeLeaves ./profiles // { users = rakeLeaves ./users; };

        suites = with common.profiles; {
          base = [ core secrets users.rking ];
          gui = [ fonts ];
          devel = [ languages.go languages.nodejs ];
        };

        imports = [ (digga.lib.importOverlays ./overlays) ];

        overlays = [ nur.overlay agenix.overlay nvfetcher.overlay ];
      };
    in
    mkFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];

      channelsConfig = { allowUnfree = true; };

      channels = mkMerge [
        (import ./darwin/channels.nix { inherit inputs common; })
        (import ./nixos/channels.nix { inherit inputs common; })
      ];

      lib = import ./lib { lib = digga.lib // nixpkgs.lib; };

      sharedOverlays = [
        (final: prev: {
          __dontExport = true;
          lib = prev.lib.extend (lfinal: lprev: { our = self.lib; });
        })
      ];

      darwin = import ./darwin { inherit self inputs common; };

      home = import ./home { inherit inputs; };

      homeConfigurations = mkHomeConfigurations
        self.darwinConfigurations;
    };
}
