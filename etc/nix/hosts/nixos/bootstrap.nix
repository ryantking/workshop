{profiles, ...}: {
  imports = with profiles; [
    core.nixos
    users.root
    users.rking
  ];

  boot.loader.systemd-boot.enable = true;

  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};
}
