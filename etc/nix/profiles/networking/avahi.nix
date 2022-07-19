{
  config,
  lib,
  pkgs,
  ...
}: {
  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
    };

    nssmdns = true;
  };
}
