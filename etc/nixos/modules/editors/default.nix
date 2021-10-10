{ config, lib, ... }:

let
  inherit (lib) mkIf mkOption types;

  cfg = config.modules.editors;
in {
  options.modules.editors = {
    default = mkOption {
      description = "Default editor";
      type = types.str;
    };
  };

  config = mkIf (cfg.default != null) {
    environment.variables.EDITOR = cfg.default;
  };
}
