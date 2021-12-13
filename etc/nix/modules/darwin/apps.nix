{ ... }:

{
  homebrew = {
    taps = [
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/cask-drivers"
      "d12frosted/emacs-plus"
      "railwaycat/emacsmacport"
    ];

    brews = [ "mas" "libvterm" ];

    casks = [
      "1password-beta"
      "adobe-creative-cloud"
      "alacritty"
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
      "Fantastical" = 975937182;
      "Tailscale" = 1475387142;
      "Telegram" = 747648890;
      "Slack" = 803453959;
    };

    # extraConfig =
    #   ''brew "emacs-mac", args: ["with-emacs-big-sur-icon", "with-mac-metal", "with-natural-title-bar", "with-starter"]'';
    extraConfig = ''
      brew "emacs-plus@28", args: ["with-elrumo1-icon", "with-imagemagick", "with-mailutils", "with-native-comp", "with-xwidgets"]'';
  };
}
