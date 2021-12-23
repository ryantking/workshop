{ self, inputs, ... }:

with inputs;
with inputs.nixos;

let inherit (digga.lib) rakeLeaves;
in rec {
  profiles = rakeLeaves ../profiles // { users = rakeLeaves ../users; };

  suites = with profiles; rec {
    base = [ core users.rking users.root ];
    nixos = base ++ [
      application.base
      application.chat
      application.misc

      fonts

      graphical.xserver
      graphical.nvidia
      graphical.kde

      hardware.audio
      hardware.bluetooth
      hardware.keyboard

      linux
      networking
      security.yubikey
    ];
  };
}
