{ self, nixos, digga, agenix, agenix-cli, nvfetcher, emacs, ... }@inputs:

let
  diggalib = digga.lib;

  common = rec {
    modules = diggalib.importExportableModules ./modules;

    profiles = diggalib.rakeLeaves ./profiles // { users = diggalib.rakeLeaves ./users; };

    suites = with profiles; {
      base = [ core users.rking ];
      gui = [ fonts ];
      devel = [ languages.go languages.nodejs ];
    };
  };
in diggalib.mkFlake {
  inherit self inputs;

  supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];

  channels =
    let
      overlays = [ agenix.overlay nvfetcher.overlay emacs.overlay ./pkgs ];
      imports = [ (diggalib.importOverlays ./overlays) ];
    in
      {
        nixos = { inherit imports overlays; };
        nixpkgs-darwin = { inherit imports overlays; };
        latest = { };
      };

  channelsConfig = {
    allowUnfree = true;
    allowUnspportedSystem = true;
  };

  lib = import ./lib { lib = diggalib // nixos.lib; };

  sharedOverlays = [
    (final: prev: {
      __dontExport = true;
      lib = prev.lib.extend (lfinal: lprev: { our = self.lib; });
      agenix-cli = inputs.agenix-cli.defaultPackage.${prev.stdenv.system};
    })
  ];

  nixos = import ./nixos (inputs // { inherit common; });

  darwin = import ./darwin (inputs // { inherit common; });

  home = import ./home inputs;

  devshell = ./shell;

  homeConfigurations = diggalib.mkHomeConfigurations (self.darwinConfigurations // self.nixosConfigurations);
}
