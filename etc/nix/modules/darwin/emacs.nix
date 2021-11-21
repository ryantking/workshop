{ pkgs, ... }:

{
  homebrew = {
    taps = [ "d12frosted/emacs-plus" ];
    brews = [ "grep" "libvterm" ];
    extraConfig = ''
      brew "emacs-plus@28", args: ["with-no-titlebar", "with-elrumo2-icon", "with-native-comp", "with-xwidgets"]
    '';
  };

  environment.extraInit = ''
    # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
    if [[ ! -d "$HOME/.config/emacs" ]]; then
      git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.config/emacs"
      DOOMDIR="$XDG_CONFIG_HOME/doom $XDG_CONFIG_HOME/emacs/bin/doom install"
    fi
  '';

  launchd.user.agents.emacs = {
    command = "emacs --fg-daemon";
    serviceConfig = { RunAtLoad = true; };
  };

  hm = {
    programs.fish.interactiveShellInit =
      "set -x PATH /usr/local/opt/grep/libexec/gnubin $PATH";

    home = {
      packages = with pkgs; [
        cmake
        sbcl
        gotests
        shellcheck
        graphviz
        gocode
        gomodifytags
        gore
        gotools
        nodePackages.marked
        rust-analyzer
      ];

      sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];
    };
  };
}
