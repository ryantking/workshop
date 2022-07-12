{suites, ...}: {
  imports = suites.base ++ suites.gui ++ suites.devel;

  networking.hostName = "trashbook";

  nixpkgs.system = "x86_64-darwin";

  nix = {
    maxJobs = 4;
    buildCores = 0;
  };

  whoami.keys.pgp.machine = "0x7B9DDE8739045EF3";
}
