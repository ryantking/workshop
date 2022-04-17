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
    emacs.url = "github:nix-community/emacs-overlay";
    rnix-lsp.url = "github:nix-community/rnix-lsp";
    treefmt.url = "github:numtide/treefmt";
  };

  outputs = { self, nixpkgs, home, digga, colors, agenix, nur, nvfetcher, emacs, ... } @ inputs:
    let
      inherit (digga.lib) mkFlake rakeLeaves importExportableModules importOverlays;

      profiles = rakeLeaves ./profiles // { users = rakeLeaves ./users; };

      common = {
        inherit profiles;

        modules = importExportableModules ./modules;

        suites = with profiles; {
          base = [ core secrets users.rking ];
          gui = [ fonts ];
          devel = [ languages.go languages.nodejs ];
        };

        imports = [ (importOverlays ./overlays) ];

        overlays = [ nur.overlay agenix.overlay nvfetcher.overlay emacs.overlay ];
      };
    in
    mkFlake
      {
        inherit self inputs;

        channels.nixpkgs-darwin = {
          imports = common.imports;
          overlays = common.overlays ++ [ ./pkgs ];
        };

        channelsConfig = { allowUnfree = true; };

        lib = import ./lib { lib = digga.lib // nixpkgs.lib; };

        sharedOverlays = [
          (final: prev: {
            __dontExport = true;
            lib = prev.lib.extend (lfinal: lprev: { our = self.lib; });
          })
        ];

        imports = [
          (import ./darwin { inherit self inputs common; })
          (import ./nixos { inherit self inputs common; })
          (import ./home { inherit self inputs common; })
        ];
      };
}
