{ self, inputs, common }:

with inputs;
with digga.lib;

{
  supportedSystems = [ "x86_64-darwin" ];

  darwin = {
    hostDefaults = {
      system = "x86_64-darwin";

      channelName = "nixpkgs-darwin";

      imports = [ common.modules (importExportableModules ./modules) ];

      modules = [
        { lib.our = self.lib; }
        digga.darwinModules.nixConfig
        home.darwinModules.home-manager
        agenix.nixosModules.age
      ];
    };

    imports = [ (importHosts ./hosts) ];

    hosts = {
      trashbook = { };
    };

    importables = rec {
      profiles = common.profiles // (rakeLeaves ./profiles);

      suites = with profiles; rec {
        base = common.suites.base ++ [ core ];
        gui = common.suites.gui ++ [ apps yabai skhd sketchybar ];
        devel = common.suites.devel;
      };
    };
  };
}
