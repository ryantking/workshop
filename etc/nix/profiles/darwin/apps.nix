{...}: {
  homebrew = {
    taps = [
      "homebrew/cask"
      "homebrew/cask-drivers"
      "homebrew/cask-versions"
    ];

    brews = ["gcc" "libgccjit" "pngpaste"];

    casks = [
      "1password-beta"
      # "adobe-acrobat-reader" # intel
      # "adobe-creative-cloud" # intel
      "alfred"
      "dash"
      "displaperture"
      "docker"
      "dropbox"
      "firefox-developer-edition"
      "karabiner-elements"
      "logitech-options"
      "kitty"
      "mactex-no-gui"
      "meetingbar"
      "pictogram"
      "qlcolorcode"
      "qlmarkdown"
      "qlstephen"
      # "qlvideo" # intel
      "quicklook-json"
      "quicklookase"
      "qutebrowser"
      "signal"
      "sonos"
      "spotify"
      "webpquicklook"
      "yubico-yubikey-personalization-gui"
      "zsa-wally"
    ];

    masApps = {
      "Tailscale" = 1475387142;
      "Telegram" = 747648890;
      "Slack" = 803453959;
    };

    extraConfig = ''
      brew "railwaycat/emacsmacport/emacs-mac", args: [
        "with-native-comp", "with-mac-metal", "with-xwidgets", "with-imagemagick", "with-rsvg",
        "with-natural-title-bar", "with-emacs-big-sur-icon", "with-starter"
      ]'';
  };

  shell.env.PATH = ["/Library/TeX/Distributions/Programs/texbin"];
}
