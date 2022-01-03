{ self, inputs, profiles, suites, ... }:

with inputs;

let
  inherit (digga.lib) rakeLeaves;

  hosts = rakeLeaves ./hosts;
  profiles = profiles // (rakeLeaves ./profiles);
  # suites = suites // (import ./suites { inherit profiles; });

  mkHost = { name, system ? "x86_64-darwin", channelName ? "nixpkgs-darwin"
    , extraSuites ? [ ] }: {
      ${name} = {
        inherit system channelName;
        output = "darwinConfigurations";
        builder = darwin.lib.darwinSystem;
        specialArgs = { inherit profiles suites; };
        modules = extraSuites
          ++ [ hosts.${name} home.darwinModules.home-manager ];
      };
    };
in (mkHost "trashbook" { extraSuites = [ ]; })
