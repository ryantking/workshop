final: prev: {
  skhd = prev.skhd.overrideAttrs (o: rec {
    buildInputs = o.buildInputs ++ [ final.makeWrapper ];
    postInstall = o.postInstall or "" + ''
      wrapProgram "$out/bin/skhd" --set SHELL /bin/bash
    '';
  });
}
