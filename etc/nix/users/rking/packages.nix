{ pkgs }:

with pkgs;[
  alejandra
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
  lnav
  most
  neovim
  nixpkgs-fmt
  nnn
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
