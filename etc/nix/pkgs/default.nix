final: prev: {
  sources = prev.callPackage (import ./generated.nix) {};
}
