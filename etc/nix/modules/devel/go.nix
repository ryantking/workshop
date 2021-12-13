{ pkgs, ... }:

{
  hm = {
    programs.go = {
      enable = true;
      package = pkgs.go_1_17;
      goPath = "go";
      goPrivate = [ "gihub.com/ryantking" "github.com/operator-framework" ];
    };

    home = {
      packages = with pkgs; [
        gocode
        gofumpt
        golangci-lint
        gomodifytags
        gopls
        gore
        gotests
        gotools
      ];

      sessionVariables = {
        GOPROXY = "https://goproxy.io,direct";
        GOSUMDB = "gosum.io+ce6e7565+AY5qEHUk/qmHc5btzW45JVoENfazw8LielDsaI+lEbq6";
      };
    };
  };
}
