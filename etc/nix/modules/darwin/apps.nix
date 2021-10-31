{ ... }:

{
  homebrew = {
    taps = [
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/cask-drivers"
    ];

    brews = [ "mas" ];

    casks = [
      "alfred"
      "bartender"
      "brave-browser"
      "discord"
      "displaperture"
      "element"
      "expressvpn"
      "ferdi-beta"
      "gpg-suite"
      "karabiner-elements"
      "logitech-options"
      "protonmail-bridge"
      "signal"
      "spotify"
      "telegram"
      "viscosity"
      "yubico-yubikey-manager"
      "yubico-yubikey-personalization-gui"
    ];

    masApps = {
      "1Password 7" = 1333542190;
      "Bear" = 1091189122;
      "Fantastical" = 975937182;
      "Telegram" = 747648890;
      "Things 3" = 904280696;
      "Slack" = 803453959;
    };
  };
}
