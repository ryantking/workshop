{ self
, common
, home
, digga
, agenix
, ...
}:

{
  hostDefaults = {
    system = "x86_64-darwin";

    channelName = "nixpkgs-darwin";

    imports = [ common.modules (digga.lib.importExportableModules ./modules) ];

    modules = [
      { lib.our = self.lib; }
      digga.darwinModules.nixConfig
      home.darwinModules.home-manager
      agenix.nixosModules.age
    ];
  };

  imports = [ (digga.lib.importHosts ./hosts) ];

  hosts = {
    trashbook = { };
  };

  importables = rec {
    profiles = common.profiles // (digga.lib.rakeLeaves ./profiles);

    suites = with profiles; rec {
      base = common.suites.base ++ [ core ];
      gui = common.suites.gui ++ [ apps yabai skhd sketchybar ];
      devel = common.suites.devel;
    };
  };
}
