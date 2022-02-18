{ ... }:

{
  homebrew.casks = [ "alacritty" ];

  system.activationScripts.postUserActivation.text = ''
    [[ -d "$HOME/.terminfo" ]] && exit 0

    echo "Installing additional terminal descriptions..."
    cd $(mktemp -d)
    curl -sLO https://invisible-island.net/datafiles/current/terminfo.src.gz
    gunzip terminfo.src.gz
    tic -xe alacritty,alacritty-direct,tmux-256color terminfo.src'';
}
