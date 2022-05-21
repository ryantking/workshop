{ pkgs, ... }:

{
  networking = {
    dns = [ "1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];

    networkmanager = {
      enable = true;
      packages = [ pkgs.networkmanager-openvpn ];
    };
  };
}
