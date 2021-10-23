final: prev:
let
  inherit (prev) fetchFromGitHub;
  inherit (prev.vimUtils) buildVimPluginFrom2Nix;
in {
  vimPlugins = prev.vimPlugins // {
    comment-nvim = buildVimPluginFrom2Nix {
      pname = "comment.nvim";
      version = "2021-10-12";
      src = fetchFromGitHub {
        owner = "numToStr";
        repo = "comment.nvim";
        rev = "6c99d540953bb42ecc84c9e1078ec3b2655fb17d";
        sha256 = "0pc7iq39b1scn0nnnqzi87n2b01jkp38n62cm71c9is4axz20yci";
      };
      meta.homepage = "https://github.com/numToStr/comment.nvim/";
    };

    cmp-look = buildVimPluginFrom2Nix {
      pname = "cmp-look";
      version = "2021-10-16";
      src = fetchFromGitHub {
        owner = "octaltree";
        repo = "cmp-look";
        rev = "6578ab3966590ca2a3c5759b9a57b30b495b3431";
        sha256 = "0yi0c2rsvrcq5hg6nirx0s44an9q194xlb525zlrh8270nrg73aq";
      };
      meta.homepage = "https://github.com/octaltree/cmp-look/";
    };

    guihua-lua = buildVimPluginFrom2Nix {
      pname = "guihua.lua";
      version = "2021-09-27";
      src = fetchFromGitHub {
        owner = "ray-x";
        repo = "guihua.lua";
        rev = "ec5ce1e3f12919639b6dbb43ec9914637e9a0db0";
        sha256 = "147x1rzlx6q3hp51p99g027im3pvkp9794d8n99m7k86mamam145";
      };
      meta.homepage = "https://github.com/ray-x/guihua.lua/";
      postInstall = "(cd lua/fzy && make)";
    };

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

    navigator-lua = buildVimPluginFrom2Nix {
      pname = "navigator.lua";
      version = "2021-10-11";
      src = fetchFromGitHub {
        owner = "ray-x";
        repo = "navigator.lua";
        rev = "36683e3646cf83bea9040527bf9746058d4210bf";
        sha256 = "16pr5ymw217vlx6sngrbz67vkwn23hzyrpzvb6hrlcfsmdp83fas";
      };
      meta.homepage = "https://github.com/ray-x/navigator.lua/";
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

    project-nvim = buildVimPluginFrom2Nix {
      pname = "project.nvim";
      version = "2021-09-29";
      src = fetchFromGitHub {
        owner = "ahmedkhalf";
        repo = "project.nvim";
        rev = "f95b92cac5d8773e676d4d1b296da7d7b872b62f";
        sha256 = "0rb9plsqps0pv60xbj6d3980iq1ds5gxh949zjhlns0h87iiamcf";
      };
      meta.homepage = "https://github.com/ahmedkhalf/project.nvim/";
    };

    tmux-nvim = buildVimPluginFrom2Nix {
      pname = "tmux.nvim";
      version = "2021-10-10";
      src = fetchFromGitHub {
        owner = "aserowy";
        repo = "tmux.nvim";
        rev = "3296fa9f0ee63cf3af8e7c52063dac14f20ff409";
        sha256 = "0q5shx18yp7lfigk4xzak8zb5rdvyzsakkakv3wbqmdzxhzsq00i";
      };
      meta.homepage = "https://github.com/aserowy/tmux.nvim/";
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

    which-key-nvim = buildVimPluginFrom2Nix {
      pname = "which-key.nvim";
      version = "2021-10-23";
      src = fetchFromGitHub {
        owner = "folke";
        repo = "which-key.nvim";
        rev = "d3032b6d3e0adb667975170f626cb693bfc66baa";
        sha256 = "sha256-mgLmwP8ci9VjRiwBedZDPXi6CjNtJy3iOJDbmSXtisk";
      };
      meta.homepage = "https://github.com/folke/which-key.nvim/";
    };
  };
}
