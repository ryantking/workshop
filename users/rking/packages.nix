{ pkgs }:

with pkgs; [
  alejandra
  agenix
  bottom
  bandwhich
  exa
  fd
  getopt
  gnugrep
  gdb
  grc
  jq
  kind
  kubectx
  krew
  lnav
  most
  neovim
  nixpkgs-fmt
  nnn
  pandoc
  podman
  procs
  rage
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
