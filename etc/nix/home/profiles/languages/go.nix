{ pkgs, ... }:

{
  shell = {
    env = {
      GOPATH = "$XDG_DATA_HOME/go";
      PATH = [ "$GOPATH/bin" "$PATH" ];
      GOPROXY = "https://goproxy.io,direct";
      GOPRIVATE = "gihub.com/ryantking,github.com/operator-framework,github.com/kubernetes-sigs";
      GOSUMDB = "gosum.io+ce6e7565+AY5qEHUk/qmHc5btzW45JVoENfazw8LielDsaI+lEbq6";
    };

    abbrs = {
      t = "go test -v";
    };
  };
}