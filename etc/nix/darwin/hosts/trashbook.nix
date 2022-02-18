{ suites, ... }:

{
  imports = with suites; base ++ gui ++ devel;

  networking.hostName = "trashbook";

  nixpkgs.system = "x86_64-darwin";

  nix = {
    maxJobs = 4;
    buildCores = 0;
  };
}
