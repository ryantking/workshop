final: prev:
let
  nodeEnv = import ./node-env.nix {
    inherit (final) stdenv lib python2 runCommand writeTextFile;
    pkgs = final;
    nodejs = final.nodejs-12_x;
    libtool = if final.stdenv.isDarwin then final.darwin.cctools else null;
  };
in {
  nodePackages = prev.nodePackages // (import ./node-packages.nix {
    inherit (final) fetchurl nix-gitignore stdenv lib fetchgit;
    inherit nodeEnv;
  });
}
