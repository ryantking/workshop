channels: final: prev: rec {
  __dontExport = true;
  inherit
    (channels.latest)
    alejandra
    cachix
    gopls
    go_1_18
    nix
    nix-direnv
    prettier
    shellcheck
    starship
    ;

  gofumpt = channels.latest.gofumpt.overrideAttrs (o: {
    inherit (channels.latest.gofumpt) src;
    runVend = true;
    vendorSha256 = "sha256-Il1E1yOejLEdKRRMqelGeJbHRjx4qFymf7N98BEdFzg=";
  });

  gotools = channels.latest.gotools.overrideAttrs (o: rec {
    inherit (channels.latest.gotools) src;
    vendorSha256 = "sha256-UJIXG8WKzazNTXoqEFlT/umC40F6z2Q5I8RfxnMbsPM=";
  });

  goimapnotify = channels.latest.goimapnotify.overrideAttrs (o: rec {
    inherit (prev.sources.goimapnotify) src;
    vendorSha256 = "sha256-DphGe9jbKo1aIfpF5kRYNSn/uIYHaRMrygda5t46svw=";
    meta.platforms = ["x86_64-linux" "x86_64-darwin"];
  });

  fennel =
    channels.latest.fennel.overrideAttrs
    (o: rec {
      inherit (prev.sources.fennel) version src;
      buildInputs = [channels.latest.lua5_4];
    });
}
