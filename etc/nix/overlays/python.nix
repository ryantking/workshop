final: prev:
let lib = prev.lib;
in rec {
  python39 = prev.python39.override {
    packageOverrides = final: prev: {
      beautifulsoup4 = prev.beautifulsoup4.overrideAttrs
        (old: { propagatedBuildInputs = lib.remove prev.lxml old.propagatedBuildInputs; });
    };
  };
  python39Packages = python39.pkgs;
}
