{ config, lib, pkgs, ... }:

{
  imports = [ ./emacs.nix ];

  homebrew = {
    casks = [
      "1password-beta"
      "adobe-acrobat-reader"
      "adobe-creative-cloud"
      "alacritty"
      "alfred"
      "displaperture"
      "docker"
      "dropbox"
      "karabiner-elements"
      "logitech-options"
      "logseq"
      "jetbrains-toolbox"
      "meetingbar"
      "pictogram"
      "protonvpn"
      "qlcolorcode"
      "qlmarkdown"
      "qlstephen"
      "qlvideo"
      "quicklook-json"
      "quicklookase"
      "signal"
      "sonos"
      "spotify"
      "webpquicklook"
      "yubico-yubikey-personalization-gui"
      "zsa-wally"
    ];

    masApps = {
      "Fantastical" = 975937182;
      "Tailscale" = 1475387142;
      "Telegram" = 747648890;
      "Slack" = 803453959;
    };
  };
}
