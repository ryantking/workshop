{
  self,
  pkgs,
  extraModulesPath,
  inputs,
  ...
}: let
  hooks = import ./hooks;

  pkgWithCategory = category: package: {inherit package category;};
  workshop = pkgWithCategory "workshop";
  linter = pkgWithCategory "linter";
  formatter = pkgWithCategory "formatter";
  docs = pkgWithCategory "docs";
  mkEnv = name: value: {inherit name value;};

  inherit
    (pkgs)
    alejandra
    editorconfig-checker
    go
    gofumpt
    golangci-lint
    gotools
    nvfetcher
    shellcheck
    statix
    ;
in {
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
    # (workshop nvfetcher)
    (workshop go)

    # (linter statix)
    # (linter golangci-lint)
    # (linter editorconfig-checker)
    # (linter shellcheck)

    # (formatter alejandra)
    # (formatter gofumpt)
  ];
}
