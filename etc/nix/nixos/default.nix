{ self, inputs, ... }:

with inputs;
with inputs.nixos;

{
  hostDefaults = {
    system = "x86_64-linux";
    channelName = "nixos";
    imports = [ (digga.lib.importExportableModules ./modules) ];
    modules = [
      { lib.our = self.lib; }
      digga.nixosModules.nixConfig
      home.nixosModules.home-manager
      bud.nixosModules.bud
      agenix.nixosModules.age
    ];
  };

  imports = [ (digga.lib.importHosts ./hosts) ];

  hosts = { trashstation = { }; };

  importables = import ./suites { inherit self inputs; };
}
