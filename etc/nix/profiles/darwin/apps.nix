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
      "gpg-suite-no-mail"
      "karabiner-elements"
      "kitty"
      "ledger-live"
      "librewolf"
      "logitech-options"
      "mactex-no-gui"
      "meetingbar"
      "monero-wallet"
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
      "thunderbird"
      "tor-browser"
      "webpquicklook"
      "yubico-yubikey-personalization-gui"
      "zsa-wally"
    ];
  };

  shell.env.PATH = ["/Library/TeX/Distributions/Programs/texbin"];
}
