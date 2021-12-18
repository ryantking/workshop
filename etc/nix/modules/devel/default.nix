{ pkgs, ... }:

{
  imports = [ ./nix.nix ./go.nix ./rust.nix ];

  config = {
    hm.home.packages = with pkgs // pkgs.nodePackages; [
      bash-language-server
      cmake
      dockerfile-language-server-nodejs
      fennel
      fnlfmt
      # gcc
      gnumake
      graphviz
      kube3d
      marked
      sbcl
      selene
      shellcheck
      shellharden
      shfmt
      stylua
      treefmt
      vscode-json-languageserver
    ];
  };
}
