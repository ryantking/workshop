final: prev: {
  spacebar = prev.spacebar.overrideAttrs (o: rec {
    inherit (prev.sources.spacebar) pname version src;
  });
}
