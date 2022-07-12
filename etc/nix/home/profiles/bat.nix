{pkgs, ...}: {
  programs.bat = {
    enable = true;
    config = {pager = "less -FR";};
  };

  home.activation."batCacheBuild" = {
    before = [];
    after = ["linkGeneration"];
    data = "${pkgs.bat}/bin/bat cache --build";
  };
}
