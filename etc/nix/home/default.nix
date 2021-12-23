{ self, inputs, ... }:

with inputs;

{
  imports = [ (digga.lib.importExportableModules ./modules) ];
  modules = [ nix-colors.homeManagerModule ];
  importables = rec {
    profiles = digga.lib.rakeLeaves ./profiles;
    suites = with profiles; rec {
      base = [
        alacritty
        kitty
        shell.bat
        shell.direnv
        shell.git
        shell.misc
        shell.packages
        shell.tmux
        shell.zsh
      ];
    };
  };

  users = {
    rking = { suites, ... }: {
      imports = suites.base;
      home.enableNixpkgsReleaseCheck = false;
    };
  };
}
