final: prev: {
  guru = prev.buildGoModule rec {
    pname = "guru";
    version = "0.1.7";

    src = prev.fetchgit {
      rev = "v${version}";
      url = "https://go.googlesource.com/tools";
      sha256 = "";
    };

    runVend = true;
    vendorSha256 = "";

    meta = {
      description = "Ask questions about go source code";
      homepage = "https://github.com/golang/tools";
    };
  };
}
