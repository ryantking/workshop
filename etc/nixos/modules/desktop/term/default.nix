{ config, lib, ... }:

let
  inherit (lib) mkIf mkOption types;

  cfg = config.modules.desktop.term;
in {
  options.modules.desktop.term = {
    default = mkOption {
      description = "Default terminal";
      type = types.str;
    };
  };

  config = mkIf (cfg.default != null) { environment.variables.TERMINAL = cfg.default; };
}
