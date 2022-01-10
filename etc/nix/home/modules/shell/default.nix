{ config, options, lib, pkgs, ... }:

with lib.types;

let
  inherit (lib) mapAttrsToList concatStringsSep concatMapStrings;
  inherit (pkgs.lib.our) mkOpt;

  cfg = config.my.shell;
in
{
  options.my.shell = {
    abbrs = mkOpt (attrsOf str) { };
    aliases = mkOpt (attrsOf str) { };

    rcFiles = mkOpt (listOf (either str path)) [ ];

    rcInit =
      let
        aliasLines = mapAttrsToList
          (n: v: ''alias ${n}="${v}"'')
          (cfg.abbrs // cfg.aliases);
      in
      mkOpt lines ''
        ${concatStringsSep "\n" aliasLines}
        ${concatMapStrings (path: "source '${path}'") cfg.rcFiles}
      '';
  };
}
