{...}: {
  homebrew = {
    taps = [
      "homebrew/cask"
      "homebrew/cask-drivers"
      "homebrew/cask-versions"
      "qmk/qmk"
    ];

    brews = ["qmk" "pngpaste"];

    casks = [
      "1password-beta"
      "adobe-acrobat-reader" # intel
      "adobe-creative-cloud" # intel
      "alfred"
      "dash"
      "discord"
      "displaperture"
      "docker"
      "dropbox"
      "railwaycat/homebrew-emacsmacport/emacs-mac"
      "firefox-developer-edition"
      "gpg-suite-no-mail"
      "karabiner-elements"
      "kitty"
      "ledger-live"
      "logitech-options"
      "mactex-no-gui"
      "meetingbar"
      "monero-wallet"
      "mymonero"
      "nextcloud"
      "pictogram"
      "postman"
      "protonmail-bridge"
      "qlcolorcode"
      "qlmarkdown"
      "qlstephen"
      "qlvideo" # intel
      "quicklook-json"
      "quicklookase"
      "signal"
      "slack"
      "sonos"
      "spotify"
      "tailscale"
      "telegram-desktop"
      "tor-browser"
      "webpquicklook"
      "yubico-yubikey-personalization-gui"
      "zsa-wally"
    ];

    # extraConfig = ''
    # cask "railwaycat/emacsmacport/emacs-mac", args: [
    # "with-native-comp", "with-mac-metal", "with-xwidgets", "with-imagemagick", "with-rsvg",
    # "with-natural-title-bar", "with-emacs-big-sur-icon", "with-starter"
    # ]'';
  };

  shell.env.PATH = ["/Library/TeX/Distributions/Programs/texbin"];
}
