{ config, options, lib, pkgs, ... }:

with lib.types;

let
  inherit (lib) mkOption mapAttrsToList concatStringsSep concatMapStrings mapAttrs isList concatMapStringsSep;
  inherit (pkgs.lib.our) mkOpt;

  cfg = config.shell;
  fmt = pkgs.formats.toml { };
in
{
  options.shell = {
    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs
        (n: v: if isList v then concatMapStringsSep ":" (x: toString x) v else (toString v));
    };

    abbrs = mkOpt (attrsOf str) { };

    aliases = mkOpt (attrsOf str) { };

    extraInit = mkOpt lines "";

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
        ${cfg.extraInit}
      '';

    prompt.starship = mkOpt fmt.type { };
  };
}
