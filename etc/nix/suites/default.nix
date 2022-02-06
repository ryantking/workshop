{ profiles, homeProfiles, ... }:

rec {
  base = [
    profiles.core
    homeProfiles.zsh
    homeProfiles.bat
    homeProfiles.git
    homeProfiles.zsh
  ];

  gui = [
    profiles.fonts
    homeProfiles.alacritty
  ];

  darwin = base ++ gui ++ [
    profiles.darwin
    homeProfiles.darwin.apps
    homeProfiles.darwin.yabai
  ];

  devel = base ++ [ homeProfiles.emacs homeProfiles.direnv homeProfiles.tmux profiles.languages.nodejs homeProfiles.languages.go ];

  personal = [ profiles.security.yubikey profiles.secrets homeProfiles.gnupg homeProfiles.tealdeer ];
}
