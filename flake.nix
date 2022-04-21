{
  description = "Ryan's NixOS configurations";

  inputs = {
    # Channels
    nixos.url = "github:nixos/nixpkgs/nixos-21.11";
    latest.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";
    nixpkgs.follows = "nixos";

    # Configuration Layers
    darwin = {
      # url = "github:ryantking/nix-darwin";
      url = "path:/Users/rking/Projects/nix/nix-darwin";
      inputs.nixpkgs.follows = "nixos";
    };
    home = {
      # url = "github:nix-community/home-manager";
      url = "path:/Users/rking/Projects/nix/home-manager";
      inputs.nixpkgs.follows = "nixos";
    };

    # Flake Utilities
    digga = {
      url = "github:divnix/digga/darwin-support";
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

    # Source Management
    nur.url = "github:nix-community/NUR";
    nvfetcher.url = "github:berberman/nvfetcher";

    # Secrets Management
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixos";
    };

    # Development Tools
    emacs.url = "github:nix-community/emacs-overlay";
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixos";
    };
    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs = {
        nixpkgs.follows = "nixos";
        naersk.follows = "naersk";
      };
    };
    treefmt.url = "github:numtide/treefmt";
  };

  outputs = { self, nixos, digga, colors, agenix, nur, nvfetcher, emacs, ... } @ inputs:
    let
      inherit (digga.lib) mkFlake rakeLeaves importExportableModules importOverlays;

      profiles = rakeLeaves ./profiles // { users = rakeLeaves ./users; };

      common = {
        inherit profiles;

        modules = importExportableModules ./modules;

        suites = with profiles; {
          base = [ core users.rking ];
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

        channelsConfig = {
          allowUnfree = true;
          allowUnspportedSystem = true;
        };

        lib = import ./lib { lib = digga.lib // nixos.lib; };

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
