{ pkgs, ... }:

{
  homebrew = {
    taps = [ "d12frosted/emacs-plus" ];
    brews = [ "libvterm" ];
    extraConfig = ''
      brew "emacs-plus@28", args: ["with-elrumo2-icon", "with-native-comp", "with-xwidgets"]
    '';
  };

  system.activationScripts.postUserActivation.text = ''
    # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
    if [[ ! -d "$HOME/.config/emacs" ]]; then
      git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.config/emacs"
      DOOMDIR="$XDG_CONFIG_HOME/doom $XDG_CONFIG_HOME/emacs/bin/doom install"
    fi
  '';

  hm.home = {
    packages = [ pkgs.coreutils-prefixed ];

    sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];
  };
}
