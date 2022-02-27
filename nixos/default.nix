{ self, inputs, common }:

with inputs;
with digga.lib;

{
  supportedSystems = [ "x86_64-linux" ];

  channels = {
    nixpkgs = {
      imports = common.imports ++ [ (importOverlays ./overlays) ];
      overlays = common.overlays;
    };

    nixpkgs-latest = { };
  };

  nixos = {
    hostDefaults = {
      system = "x86_64-linux";
      channelName = "nixpkgs";
      imports = [ ((importExportableModules ./modules) // common.modules) ];
      modules = [
        { lib.our = self.lib; }

        digga.nixosModules.nixConfig
        home.nixosModules.home-manager
      ];
    };

    imports = [ (importHosts ./hosts) ];

    hosts = {
      trashsite-master = { };
    };

    importables = rec {
      profiles = common.profiles // (rakeLeaves ./profiles);
      suites = with profiles; rec {
        base = common.suites.base ++ [ core ];
        gui = common.suites.gui ++ [ apps yabai ];
        devel = common.suites.devel ++ [ gpg ];
      };
    };
  };
}
