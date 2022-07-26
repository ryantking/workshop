{suites, ...}: {
  imports = suites.desktop;

  networking.hostName = "shoyobook";

  nixpkgs.system = "aarch64-darwin";

  nix = {
    maxJobs = 4;
    buildCores = 0;
  };
}
