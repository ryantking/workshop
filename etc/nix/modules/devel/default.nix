{ pkgs, ... }:

{
  imports = [ ./nix.nix ./go.nix ./rust.nix ];

  config = {
    hm.home.packages = with pkgs; [
      cmake
      fennel
      fnlfmt
      gcc
      gnumake
      graphviz
      nodePackages.bash-language-server
      nodePackages.marked
      nodePackages.vscode-json-languageserver
      sbcl
      selene
      shellcheck
      shellharden
      shfmt
      stylua
      treefmt
    ];
  };
}
