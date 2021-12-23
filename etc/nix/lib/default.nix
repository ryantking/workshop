{ lib }:

lib.makeExtensible (self: {
  isDarwin = system: (builtins.elem system lib.platforms.darwin);
  homePrefix = system: if self.isDarwin system then "/Users" else "/home";

  mkOpt = type: default: lib.mkOption { inherit type default; };

  mkOpt' = type: default: description: lib.mkOption { inherit type default description; };

  mkBoolOpt = default:
    lib.mkOption {
      inherit default;
      type = lib.types.bool;
      example = true;
    };
})
