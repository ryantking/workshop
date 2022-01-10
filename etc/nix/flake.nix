{
  description = "Ryan's NixOS configurations";

  inputs = {
    # Package Sets
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-latest.url = "github:nixos/nixpkgs/master";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";
    nixpkgs.follows = "nixpkgs-stable";

    # System Management
    darwin = {
      url = "github:ryantking/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    home = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    # Flake Utilities
    digga = {
      url = "github:montchr/digga?ref=feature/darwin-hosts-support";
      inputs = {
        nixpkgs.follows = "nixpkgs-stable";
        nixlib.follows = "nixpkgs-stable";
        home-manager.follows = "home";
      };
    };
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    colors.url = "github:misterio77/nix-colors";

    # Source Management
    nur.url = "github:nix-community/NUR";
    nvfetcher.url = "github:berberman/nvfetcher";

    # Secrets Management
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };
    agenix-cli.url = "github:cole-h/agenix-cli";

    # Development Tools
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    emacs.url = "github:nix-community/emacs-overlay";
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

    # Yabai
    yabai = {
      url = "github:koekeishiya/yabai/master";
      flake = false;
    };
    spacebar.url = "github:cxa/spacebar";
  };

  outputs = { self, nixpkgs-unstable, home, digga, utils, colors, agenix, nur, nvfetcher, emacs, neovim-nightly, ... } @ inputs:
    with inputs;
    let
      inherit (builtins) attrValues;
      inherit (digga.lib) rakeLeaves flattenTree;

      hosts = rakeLeaves ./hosts;
      profiles = rakeLeaves ./profiles;
      homeProfiles = rakeLeaves ./home/profiles;
      suites = import ./suites { inherit profiles homeProfiles; };

      mkNixosHost = name:
        { system ? "x86_64-linux"
        , channelName ? "nixpkgs-stable"
        , extraSuites ? [ ]
        }: {
          ${name} = {
            inherit system channelName;
            specialArgs = { inherit profiles suites; };
            modules = suites.base ++ extraSuites ++ [
              hosts.${name}
              home.nixosModules.home-manager
              agenix.nixosModules.age
            ];
          };
        };

      mkDarwinHost = name:
        { system ? "x86_64-darwin"
        , channelName ? "nixpkgs-darwin"
        , extraSuites ? [ ]
        }: {
          ${name} = {
            inherit system channelName;
            output = "darwinConfigurations";
            builder = darwin.lib.darwinSystem;
            specialArgs = { inherit profiles suites; };
            modules = suites.base ++ extraSuites
              ++ [ hosts.${name} home.darwinModules.home-manager ];
          };
        };

      mkHosts = hosts: (builtins.foldl' (a: b: a // b) { } hosts);
    in
    utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig = { allowUnfree = true; };

      channels = import ./channels { inherit self inputs; };

      lib = import ./lib { lib = digga.lib // nixpkgs-unstable.lib; };

      sharedOverlays = [
        (final: prev: {
          __dontExport = true;
          inherit inputs;
          lib = prev.lib.extend (lfinal: lprev: { our = self.lib; });
        })

        agenix.overlay
        nur.overlay
        nvfetcher.overlay
        emacs.overlay
        neovim-nightly.overlay
      ];

      hostDefaults = {
        extraArgs = { inherit utils inputs; };
        specialArgs = { inherit suites profiles homeProfiles; };
        modules = [ ./users/rking colors.homeManagerModule ]
          ++ (attrValues (flattenTree (rakeLeaves ./modules)))
          ++ (attrValues (flattenTree (rakeLeaves ./home/modules)));
      };

      hosts = with suites;
        mkHosts [
          (mkDarwinHost "trashbook" {
            extraSuites = (suites.darwin ++ devel ++ personal);
          })
          (mkNixosHost "trashstation" { extraSuites = [ ]; })
        ];

      homeConfigurations = digga.lib.mkHomeConfiguration
        (self.nixosConfigurations // self.darwinConfigurations);
    };
}
