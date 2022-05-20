final: prev: {
  goimapnotify = prev.buildGoModule {
    inherit (prev.sources.goimapnotify) pname version src;
    inherit (prev.goimapnotify) postPatch;

    runVend = false;
    vendorSha256 = "sha256-DphGe9jbKo1aIfpF5kRYNSn/uIYHaRMrygda5t46svw=";

    meta = {
      inherit (prev.goimapnotify.meta) description homepage license;
      platforms = [ "x86_64-linux" "x86_64-darwin" ];
    };
  };
}
