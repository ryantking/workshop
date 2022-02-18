{ self, inputs, common, ... }:

with inputs;

let
  inherit (digga.lib) rakeLeaves importHosts importExportableModules;
in
{
  hostDefaults = {
    system = "x86_64-darwin";
    channelName = "nixpkgs-darwin";
    imports = [ ((importExportableModules ./modules) // common.modules) ];
    modules = [
      { lib.our = self.lib; }

      digga.darwinModules.nixConfig
      home.darwinModules.home-manager
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
      gui = common.suites.gui ++ [ apps yabai ];
      devel = common.suites.devel ++ [ gpg ];
    };
  };
}
