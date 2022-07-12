{pkgs, ...}: {
  shell = {
    env = {
      GOPATH = "$XDG_DATA_HOME/go";
      PATH = ["$GOPATH/bin" "$PATH"];
    };

    abbrs = {
      t = "go test -v";
    };
  };
}
