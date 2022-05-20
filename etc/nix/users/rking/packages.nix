{ pkgs }:
with pkgs; [
  alejandra
  agenix-cli
  bottom
  bandwhich
  exa
  fd
  getopt
  gnugrep
  gdb
  grc
  hugo
  jq
  kind
  kubectx
  krew
  lnav
  most
  neovim
  nixpkgs-fmt
  nnn
  page
  pandoc
  podman
  procs
  (ripgrep.override { withPCRE2 = true; })
  ripgrep-all
  sd
  shellcheck
  shfmt
  tealdeer
  treefmt
  universal-ctags
  wally-cli
  yamllint
  yq
]
