{ ... }:

{
  homebrew = {
    taps = [
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/cask-drivers"
      "d12frosted/emacs-plus"
    ];

    brews = [ "mas" "libvterm" ];

    casks = [
      "adobe-creative-cloud"
      "alacaritty"
      "alfred"
      "bartender"
      "bluejeans"
      "brave-browser"
      "discord"
      "displaperture"
      "docker"
      "dropbox"
      "element"
      "ferdi-beta"
      "goland"
      "gpg-suite"
      "karabiner-elements"
      "kitty"
      "logitech-options"
      "logseq"
      "obsidian"
      "pictogram"
      "protonvpn"
      "signal"
      "spotify"
      "telegram"
      "zsa-wally"
    ];

    masApps = {
      "1Password 7" = 1333542190;
      "Fantastical" = 975937182;
      "Tailscale" = 1475387142;
      "Telegram" = 747648890;
      "Slack" = 803453959;
    };

    extraConfig =
      ''brew "emacs-plus", args: ["with-elrumo2-icon", "with-xwidgets", "without-imagemagick"]'';
  };
}
