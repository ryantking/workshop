{ pkgs }:

with pkgs;[
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
