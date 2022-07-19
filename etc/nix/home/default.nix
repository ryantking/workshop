{
  self,
  digga,
  colors,
  ...
}: {
  imports = [(digga.lib.importExportableModules ./modules)];

  modules = [colors.homeManagerModule];

  importables = rec {
    profiles = digga.lib.rakeLeaves ./profiles;
    suites = with profiles; rec {
      base = [git gpg ssh themes.nord zsh yubikey];
      gui = [kitty];
      devel = [bat direnv emacs languages.go tealdeer];
    };
  };

  users = {
    rking = {suites, ...}: {
      imports = suites.base ++ suites.gui ++ suites.devel;
    };
  };
}
