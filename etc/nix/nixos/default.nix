{ self
, common
, home
, digga
, agenix
, ...
}:

 {
  hostDefaults = {
    system = "x86_64-linux";
    channelName = "nixos";
    imports = [ ((digga.lib.importExportableModules ./modules) // common.modules) ];
    modules = [
      { lib.our = self.lib; }
      digga.nixosModules.nixConfig
      home.nixosModules.home-manager
      agenix.nixosModules.age
    ];
  };

  imports = [ (digga.lib.importHosts ./hosts) ];

  importables = rec {
    profiles = common.profiles // (digga.lib.rakeLeaves ./profiles);
    suites = with profiles; rec {
      base = common.suites.base ++ [ core ];
      gui = common.suites.gui;
      devel = common.suites.devel ++ [ gpg ];
    };
  };
}
