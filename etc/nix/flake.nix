{
  description = "Ryan's NixOS configurations";

  inputs = {
    # Channels
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-latest.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";

    # Flake Utilities
    digga = {
      url = "github:ryantking/digga";
      inputs = {
        nixpkgs.follows = "nixpkgs-latest";
        nixlib.follows = "nixpkgs-latest";
        home-manager.follows = "home";
      };
    };
    darwin.follows = "digga/darwin";
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
    rnix-lsp.url = "github:nix-community/rnix-lsp";
    treefmt.url = "github:numtide/treefmt";
    tmux-fzf = {
      url = "github:sainnhe/tmux-fzf";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home, digga, colors, agenix, nur, nvfetcher, ... } @ inputs:
    let
      inherit (digga.lib) rakeLeaves importExportableModules importOverlays;

      profiles = rakeLeaves ./profiles // { users = rakeLeaves ./users; };

      /*  Creates a flake based on a shared configuration and a list of system-specific configurations.
        The first argument is common values used in the configurations such as modules, suites, and overlays.

        Type: mkFlake :: a -> [b] -> c

        Example:
        mkFlake { suites = { ... }} { inherit self inputs; } [ ./nixos ./darwin ./home ]
        => Flake
      */
      mkFlake = common: sharedConfig: systemConfigPaths:
        let
          inherit (nixpkgs.lib) mkMerge pipe;
          inherit (digga.lib) mkFlake;

          mkSystemConfig = path: import path { inherit self inputs common; };
        in
        mkFlake sharedConfig // mkMerge ([{ inherit self inputs; }] ++ (map mkSystemConfig systemConfigPaths));

      common = {
        inherit profiles;

        modules = importExportableModules ./modules;

        suites = with profiles; {
          base = [ core secrets users.rking ];
          gui = [ fonts ];
          devel = [ languages.go languages.nodejs ];
        };

        imports = [ (importOverlays ./overlays) ];

        overlays = [ nur.overlay agenix.overlay nvfetcher.overlay ];
      };
    in
    digga.lib.mkFlake
      ({
        inherit self inputs;

        supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];

        channelsConfig = { allowUnfree = true; };

        lib = import ./lib { lib = digga.lib // nixpkgs.lib; };

        sharedOverlays = [
          (final: prev: {
            __dontExport = true;
            lib = prev.lib.extend (lfinal: lprev: { our = self.lib; });
          })
        ];
      }
      //
      (nixpkgs.lib.mkMerge [
        { inherit self inputs; }
        (import ./nixos { inherit self inputs common; })
        (import ./darwin { inherit self inputs common; })
        (import ./home { inherit self inputs common; })
      ]));
}
