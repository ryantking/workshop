{
  self,
  nixos,
  home,
  digga,
  deploy,
  ragenix,
  nvfetcher,
  emacs,
  rnix-lsp,
  ...
} @ inputs': let
  inputs =
    inputs'
    // {
      emacs =
        inputs'.emacs
        // {
          overlay = self.lib.overlayNullProtector inputs'.emacs.overlay;
        };
    };

  common = rec {
    modules = digga.lib.importExportableModules ./modules;

    profiles = digga.lib.rakeLeaves ./profiles // {users = digga.lib.rakeLeaves ./users;};

    suites = with profiles; {
      base = [core users.ryan yubikey];
      gui = [fonts];
      devel = [languages.go languages.nodejs];
    };
  };
in
  digga.lib.mkFlake {
    inherit self inputs;

    supportedSystems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];

    channels = let
      overlays = [];
      imports = [(digga.lib.importOverlays ./overlays)];
    in {
      nixos = {inherit imports overlays;};
      nixpkgs-darwin = {inherit imports overlays;};
      latest = {};
    };

    channelsConfig = {
      allowUnfree = true;
      allowUnspportedSystem = true;
      allowBroken = true;
    };

    lib = import ./lib {lib = digga.lib // nixos.lib;};

    sharedOverlays = [
      (final: prev: {
        __dontExport = true;
        lib = prev.lib.extend (lfinal: lprev: {our = self.lib;});
        rnix-lsp = rnix-lsp.defaultPackage."${prev.stdenv.system}";
        deploy = deploy.defaultPackage."${prev.stdenv.system}".deploy-rs;
      })

      ragenix.overlays.default
      nvfetcher.overlay
      deploy.overlay

      (import ./pkgs)
    ];

    nixos = {
      hostDefaults = {
        system = "x86_64-linux";
        channelName = "nixos";
        imports = [(digga.lib.importExportableModules ./modules/common)];
        modules = [
          {lib.our = self.lib;}
          digga.nixosModules.nixConfig
          digga.nixosModules.bootstrapIso
          home.nixosModules.home-manager
          ragenix.nixosModules.age
        ];
      };

      imports = [(digga.lib.importHosts ./hosts/nixos)];

      hosts = {
        bootstrap = {};
        triangle-vm = {};
      };

      importables = rec {
        profiles =
          digga.lib.rakeLeaves ./profiles
          // {
            users = digga.lib.rakeLeaves ./users;
          };

        suites = with profiles; rec {
          base = [
            boot
            core.common
            core.nixos
            users.ryan
            users.root
          ];

          desktop =
            base
            ++ [
              gui
              fonts
              dconf
              networking.common
              networking.tailscale
              networking.avahi
              hardware.audio
              hardware.bluetooth
              hardware.keyboard
              yubikey
            ];
        };
      };
    };

    darwin = {
      hostDefaults = {
        system = "aarch64-darwin";

        channelName = "nixpkgs-darwin";

        imports = [
          common.modules
          (digga.lib.importExportableModules ./modules/common)
          (digga.lib.importExportableModules ./modules/darwin)
        ];

        modules = [
          {lib.our = self.lib;}
          digga.darwinModules.nixConfig
          home.darwinModules.home-manager
          ragenix.nixosModules.age
        ];
      };

      imports = [(digga.lib.importHosts ./hosts/darwin)];

      hosts = {
        shoyobook = {};
      };

      importables = rec {
        profiles =
          digga.lib.rakeLeaves ./profiles
          // {
            users = digga.lib.rakeLeaves ./users;
          };

        suites = with profiles; rec {
          base = [
            core.common
            core.darwin
            users.ryan
          ];

          desktop =
            base
            ++ [
              fonts
              darwin.apps
              darwin.fonts
              darwin.yabai
              darwin.hammerspoon
              darwin.sketchybar
              languages.go
              languages.nodejs
            ];
        };
      };
    };

    home = import ./home inputs;

    devshell = ./shell;

    homeConfigurations = digga.lib.mkHomeConfigurations (digga.lib.collectHosts self.nixosConfigurations self.darwinConfigurations);

    deploy.nodes = digga.lib.mkDeployNodes self.nixosConfigurations {};
  }
