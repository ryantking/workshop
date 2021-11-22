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
      "protonvpn"
      "signal"
      "spotify"
      "telegram"
      "viscosity"
      "yubico-yubikey-manager"
      "yubico-yubikey-personalization-gui"
      "zsa-wally"
    ];

    extraConfig = ''
      brew "emacs-plus@28", args: ["with-elrumo2-icon", "with-native-comp", "with-xwidgets"]
    '';

    masApps = {
      "1Password 7" = 1333542190;
      "Bear" = 1091189122;
      "Fantastical" = 975937182;
      "Tailscale" = 1475387142;
      "Telegram" = 747648890;
      "Things 3" = 904280696;
      "Slack" = 803453959;
    };
  };
}
