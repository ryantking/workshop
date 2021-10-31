{ pkgs, ... }:

{
  hm = {
    programs.go = {
      enable = true;
      goPrivate = [ "github.com/ryantking" "github.com/operator-framework" ];
    };

    home.packages = with pkgs; [ gopls gofumpt golangci-lint ];
  };
}
