{ pkgs, ... }:

{
  system.activationScripts.postUserActivation.text = ''
    command -v brew > /dev/null ${pkgs.bash}/bin/bash -c "$(${pkgs.curl}/bin/curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"'';

  homebrew = {
    enable = true;
    autoUpdate = true;
    cleanup = "zap";
    global = {
      brewfile = true;
      noLock = true;
    };

    taps = [
      "homebrew/bundle"
      "homebrew/core"
      "homebrew/services"
      "beeftornado/rmtree"
      "khanhas/tap"
      "teamookla/speedtest"
    ];

    brews =
      [ "brew-rmtree" "languagetool" "speedtest" "spicetify-cli" "tailscale" "thefuck" "ykman" ];
  };
}
