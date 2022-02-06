{ options, lib, pkgs, ... }:

let
  inherit (builtins) hasAttr;
  inherit (lib) mkMerge optionals optionalAttrs;
  inherit (pkgs.stdenv) isDarwin isLinux;
in mkMerge [
  {
    my = {
      user.packages = with pkgs; (optionals isLinux [go_1_17]);

      shell.abbrs = {
        t = "go test -v";
      };

      env = {
        GOPATH = "$XDG_DATA_HOME/go";
        PATH = [ "$GOPATH/bin" "$PATH" ];
        GOPROXY = "https://goproxy.io,direct";
        GOPRIVATE = "gihub.com/ryantking,github.com/operator-framework,github.com/kubernetes-sigs";
        GOSUMDB = "gosum.io+ce6e7565+AY5qEHUk/qmHc5btzW45JVoENfazw8LielDsaI+lEbq6";
      };
    };
  }
  (optionalAttrs (hasAttr "homebrew" options) {
    homebrew.brews = [ "go" ];
  })
]
