final: prev: {
  yabai = prev.yabai.overrideAttrs (o: rec {
    inherit (prev.sources.yabai) pname version src;
    buildSymlinks = prev.runCommand "build-symlinks" { } ''
      mkdir -p $out/bin
      ln -s /usr/bin/xcrun /usr/bin/xcodebuild /usr/bin/tiffutil /usr/bin/qlmanage $out/bin
    '';

    nativeBuildInputs = [ buildSymlinks ];
  });
}
