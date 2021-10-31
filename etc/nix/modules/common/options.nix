{ config, lib, options, ... }:

let inherit (lib) mkAliasDefinitions mkOption types;
in {
  options = {
    user = mkOption {
      description = "Primary user configuration";
      type = types.attrs;
      default = { };
    };

    hm = mkOption {
      description = "Primary user's home-manager configuration";
      type = types.attrs;
      default = { };
    };
  };

  config = {
    users.users.${config.user.name} = mkAliasDefinitions options.user;

    home-manager.users.${config.user.name} = mkAliasDefinitions options.hm;
  };
}
