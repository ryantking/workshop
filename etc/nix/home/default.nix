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
        base = [ alacritty bat direnv git gpg languages.go tealdeer zsh emacs ssh ];
      };
    };

    users = {
      rking = { suites, ... }: { imports = suites.base; };
    };
  };

  homeConfigurations = mkHomeConfigurations (self.darwinConfigurations // self.nixosConfigurations);
}
