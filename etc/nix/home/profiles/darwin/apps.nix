{ config, lib, pkgs, ... }:

{
  imports = [ ./yabai.nix ];

  homebrew = {
    casks = [
      "1password-beta"
      "adobe-acrobat-reader"
      "adobe-creative-cloud"
      "alfred"
      "displaperture"
      "dropbox"
      "karabiner-elements"
      "logitech-options"
      "logseq"
      "pictogram"
      "protonvpn"
      "qlcolorcode"
      "qlmarkdown"
      "qlstephen"
      "qlvideo"
      "quicklook-json"
      "quicklookase"
      "signal"
      "spotify"
      "webpquicklook"
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
