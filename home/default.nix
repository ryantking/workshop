{ self, inputs, ... }:

with inputs;
with digga.lib;

{
  home = {
    imports = [ (importExportableModules ./modules) ];

    modules = [ colors.homeManagerModule ];

    importables = rec {
      profiles = rakeLeaves ./profiles;
      suites = with profiles; rec {
        base = [ alacritty bat direnv emacs git gpg languages.go ssh tealdeer tmux zsh ];
      };
    };

    users = {
      rking = { suites, ... }: { imports = suites.base; };
    };
  };

  homeConfigurations = mkHomeConfigurations (self.darwinConfigurations // self.nixosConfigurations);
}
