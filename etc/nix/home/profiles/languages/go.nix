{pkgs, ...}: {
  home.packages = with pkgs; [go gotools gofumpt];

  shell = {
    env = {
      GOPATH = "$XDG_DATA_HOME/go";
      PATH = ["$GOPATH/bin" "$PATH"];
    };
  };
}
