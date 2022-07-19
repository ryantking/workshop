{
  config,
  pkgs,
  ...
}: {
  home-manager.sharedModules = [
    {
      home.stateVersion = "21.11";
    }
  ];
}
