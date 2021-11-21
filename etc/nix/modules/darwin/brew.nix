{ pkgs, ... }:

let
  checkBrew = "command -v brew > /dev/null";
  installBrew = ''
    ${pkgs.bash}/bin/bash -c "$(${pkgs.curl}/bin/curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"'';
in {
  environment.extraInit = "${checkBrew} || ${installBrew}";

  homebrew = {
    enable = true;
    autoUpdate = true;
    global = {
      brewfile = true;
      noLock = true;
    };

    taps = [
      "beeftornado/rmtree"
      "homebrew/bundle"
      "homebrew/core"
      "homebrew/services"
      "teamookla/speedtest"
      "khanhas/tap"
    ];

    brews = [
      "beeftornado/rmtree/brew-rmtree"
      "languagetool"
      "spicetify-cli"
      "teamookla/speedtest/speedtest"
      "tailscale"
      "ykman"
    ];
  };
}
