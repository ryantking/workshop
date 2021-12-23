{ pkgs, ... }:

{
  home.packages = with pkgs; [
    bandwhich
    fd
    getopt
    gnugrep
    grc
    lnav
    most
    neovim
    pandoc
    podman
    procs
    (ripgrep.override { withPCRE2 = true; })
    ripgrep-all
    sd
    shellcheck
    shfmt
    starship
    tealdeer
    treefmt
    universal-ctags
    wally-cli
    yamllint
    yq
  ];
}
