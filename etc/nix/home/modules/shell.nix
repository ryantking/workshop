{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  inherit (pkgs.lib.our) mkOpt;
  fmt = pkgs.formats.toml {};

  cfg = config.shell;
in {
  options.shell = {
    env = lib.mkOption {
      type = types.attrsOf (types.oneOf [types.str types.path (types.listOf (types.either types.str types.path))]);
      apply =
        lib.mapAttrs
        (n: v:
          if lib.isList v
          then lib.concatMapStringsSep ":" (x: toString x) v
          else (toString v));
    };

    abbrs = mkOpt (types.attrsOf types.str) {};

    aliases = mkOpt (types.attrsOf types.str) {};

    extraInit = mkOpt types.lines "";

    rcFiles = mkOpt (types.listOf (types.either types.str types.path)) [];

    rcInit = let
      aliasLines =
        lib.mapAttrsToList
        (n: v: ''alias ${n}="${v}"'')
        (cfg.abbrs // cfg.aliases);
    in
      mkOpt types.lines ''
        ${lib.concatStringsSep "\n" aliasLines}
        ${lib.concatMapStrings (path: "source '${path}'") cfg.rcFiles}
        ${cfg.extraInit}
      '';

    prompt.starship = mkOpt fmt.type {};
  };
}
