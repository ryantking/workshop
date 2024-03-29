{
  self,
  pkgs,
  extraModulesPath,
  inputs,
  ...
}: let
  hooks = import ./hooks;

  pkgWithCategory = category: package: {inherit package category;};
  linter = pkgWithCategory "linter";
  formatter = pkgWithCategory "formatter";
  docs = pkgWithCategory "docs";
  workshop = pkgWithCategory "workshop";
  mkEnv = name: value: {inherit name value;};

  inherit
    (pkgs)
    agenix
    alejandra
    cachix
    editorconfig-checker
    go
    gofumpt
    golangci-lint
    gotools
    nixUnstable
    nvfetcher
    shellcheck
    shellharden
    statix
    treefmt
    yarn
    ;
in
  {
    _file = toString ./.;

    imports = ["${extraModulesPath}/git/hooks.nix"];
    git = {inherit hooks;};

    name = "workshop";
    motd = "{7}Welcome to devshell{reset}\n$(type -p menu &>/dev/null && menu)\n";

    env = [
      (mkEnv "name" "workshop")
      (mkEnv "GOPRIVATE" "github.com/ryantking/workshop")
      (mkEnv "GOSUMDB" "gosum.io+ce6e7565+AY5qEHUk/qmHc5btzW45JVoENfazw8LielDsaI+lEbq6")
      (mkEnv "GONOSUMDB" "github.com/ryantking/workshop,gopkg.in/ini.v1")
    ];

    packages = [gotools];

    commands = [
      (workshop nixUnstable)
      (workshop agenix)
      (workshop cachix)
      (workshop nvfetcher)
      (workshop go)
      (workshop yarn)

      (linter statix)
      (linter golangci-lint)
      (linter editorconfig-checker)
      (linter shellcheck)

      (formatter treefmt)
      (formatter alejandra)
      (formatter gofumpt)
    ];
  }
  ssh://git@github.com
