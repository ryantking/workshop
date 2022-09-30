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
      base = [shell git gpg ssh themes.nord yubikey];
      gui = [kitty];
      devel = [bat direnv emacs languages.go mail secrets tealdeer];
    };
  };

  users = {
    rking = {suites, ...}: {
      imports = suites.base ++ suites.gui ++ suites.devel;
    };
  };
}
