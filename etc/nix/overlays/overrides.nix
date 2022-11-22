channels: final: prev: rec {
  __dontExport = true;
  inherit
    (channels.latest)
    alejandra
    cachix
    go
    gofumpt
    gopls
    gotools
    starship
    ;

  fennel = channels.latest.lua53Packages.fennel;

  # fennel =
  #   channels.latest.fennel.overrideAttrs
  #   (o: rec {
  #     inherit (prev.sources.fennel) version src;
  #     buildInputs = [channels.latest.lua5_2];
  #   });

  goimapnotify = channels.latest.goimapnotify.overrideAttrs (o: rec {
    inherit (prev.sources.goimapnotify) src;
    vendorSha256 = "sha256-DphGe9jbKo1aIfpF5kRYNSn/uIYHaRMrygda5t46svw=";
    meta.platforms = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
  });
}
