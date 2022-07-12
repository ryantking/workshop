{...}: {
  homebrew = {
    taps = ["railwaycat/emacsmacport"];
    brews = ["pngpaste"];

    casks = [
      "1password-beta"
      "adobe-acrobat-reader"
      "adobe-creative-cloud"
      "alacritty"
      "alfred"
      "dash"
      "displaperture"
      "docker"
      "dropbox"
      "firefox-developer-edition"
      "karabiner-elements"
      "logitech-options"
      "logseq"
      "jetbrains-toolbox"
      "kitty"
      "mactex-no-gui"
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

    extraConfig = ''
      brew "emacs-mac", args: [
        "with-native-comp", "with-mac-metal", "with-xwidgets", "with-imagemagick", "with-rsvg",
        "with-natural-title-bar", "with-emacs-big-sur-icon", "with-starter"
      ]'';
  };

  shell.env.PATH = ["/Library/TeX/Distributions/Programs/texbin"];
}
