{ config, ... }:

let
  hosts = builtins.fromTOML (builtins.readFile "${config.workshop.configHome}/hosts.toml")
in {
  
}
