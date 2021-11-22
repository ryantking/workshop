{ pkgs }:

let
  inherit (pkgs) fetchFromGitHub;
  inherit (pkgs.vimUtils) buildVimPluginFrom2Nix;
in {
  hotpot-nvim = buildVimPluginFrom2Nix {
    pname = "hotpot.nvim";
    version = "2021-09-14";
    src = fetchFromGitHub {
      owner = "rktjmp";
      repo = "hotpot.nvim";
      rev = "1335f873a20f58ae6b9be52bd05d8cc4548fe5aa";
      sha256 = "0k4j9r92sh65m64qji1jb1zi3dfacqng8ck55fj9s25w06ld7zc3";
    };
    meta.homepage = "https://github.com/rktjmp/hotpot.nvim/";
  };

  impatient-nvim = buildVimPluginFrom2Nix {
    pname = "impatient.nvim";
    version = "2021-09-29";
    src = fetchFromGitHub {
      owner = "lewis6991";
      repo = "impatient.nvim";
      rev = "561b86e5602def047010d451c3e7977a65982788";
      sha256 = "01yx763h5mc5fxdzyyhw8p8hq3l66m7spdqw2dyb83n78b6l11m6";
    };
    meta.homepage = "https://github.com/lewis6991/impatient.nvim/";
  };

  nvim-lua = buildVimPluginFrom2Nix {
    pname = "nvim.lua";
    version = "2019-12-05";
    src = fetchFromGitHub {
      owner = "norcalli";
      repo = "nvim.lua";
      rev = "5d57be0b6eea6c06977b1c5fe0752da909cf4154";
      sha256 = "05cqrxlf0yq0dxlsyc54vg4396ss7rmgs4flijz31qb9262ipijy";
    };
    meta.homepage = "https://github.com/norcalli/nvim.lua/";
  };

  packer-nvim = buildVimPluginFrom2Nix {
    pname = "packer.nvim";
    version = "10-18-21";
    src = fetchFromGitHub {
      owner = "wbthomason";
      repo = "packer.nvim";
      rev = "797f15afd80dcfe213d421e969f9f5f62af3a728";
      sha256 = "sha256-8nRQ+wOUKGQTkPZjpqiC8tvFXaGZfyH93yv9pfxxJv4=";
    };
    meta.homepage = "https://github.com/wbthomasan/packer.nvim";
  };

  zest-nvim = buildVimPluginFrom2Nix {
    pname = "zest.nvim";
    version = "2021-10-06";
    src = fetchFromGitHub {
      owner = "tsbohc";
      repo = "zest.nvim";
      rev = "06cd8b2c35f0cda66f348fa3a6225c746cad2edc";
      sha256 = "1zqlcy0d3v3bgbma7jnf94dnhbcx3fc3s466rcp04751ihqmc0jr";
    };
    meta.homepage = "https://github.com/tsbohc/zest.nvim/";
  };
}
