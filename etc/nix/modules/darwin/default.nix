{ pkgs, ... }:

{
  imports = [ ./brew.nix ./apps.nix ./yabai.nix ./alacritty.nix ];

  config = {
    system.stateVersion = 4;
    programs.zsh.enable = true;
  };
}
