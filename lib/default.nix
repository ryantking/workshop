{ lib }:

lib.makeExtensible (self: {
  mkOpt = type: default: lib.mkOption { inherit type default; };

  mkOpt' = type: default: description: lib.mkOption { inherit type default description; };

  mkNerdFamily = family: (builtins.replaceStrings [ " " ] [ "" ] family);

  mkAnsiAlias = cmd: "TERM=\${TERM_ANSI=-$TERM} ${cmd}";
})
