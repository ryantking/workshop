{ pkgs, lib, ... }:

{
  programs.fish.enable = true;
  environment.shells = [ pkgs.fish ];
  user.shell = pkgs.fish;

  users = lib.optionalAttrs pkgs.stdenvNoCC.isLinux { defaultUserShell = pkgs.fish; };

  hm.programs.fish = {
    enable = true;

    shellAbbrs = {
      g = "git";
      m = "make";
      t = "tmux";
    };

    shellAliases = {
      cat = "bat --plain";
      tree = "exa -T";
      nvim = "nvim --startuptime /tmp/nvim-startuptime";
      ".." = "cd ..";
      ta = "tmux a";
      tat = "tmux a -t";
    };

    plugins = with pkgs.fishPlugins; [
      {
        name = "brew";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-brew";
          rev = "f4ee0c6d07a8902c1597d89c0e3c4f62c6044d1a";
          sha256 = "sha256-Lo/eLHbRBqV6d7h0sfPV5CwlVzxEqeIiDItsEY0iDR0=";
        };
      }
      {
        name = "cd";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-cd";
          rev = "5d76c4f9589f5d43314aff2fa06499eb6b9078fe";
          sha256 = "sha256-AyzsZtulusGbRQUCgKJtw5NzK5rAHsYGG1Ye/saK9+o=";
        };
      }
      {
        name = "forgit";
        src = forgit.src;
      }
      {
        name = "foreign-env";
        src = foreign-env.src;
      }
      {
        name = "fzf";
        src = fzf-fish.src;
      }
      {
        name = "grc";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-grc";
          rev = "a5472aca6f26ca793ee76d7451dae553875a1f1d";
          sha256 = "sha256-w8/ByVdgkXVax2Du6TGvf/VnIu0Min2zQbMaQmsZwSQ=";
        };
      }
      {
        name = "osx";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-osx";
          rev = "27039b251201ec2e70d8e8052cbc59fa0ac3b3cd";
          sha256 = "sha256-jSUIk3ewM6QnfoAtp16l96N1TlX6vR0d99dvEH53Xgw=";
        };
      }
      {
        name = "pure";
        src = pure.src;
      }
      {
        name = "rustup";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-rustup";
          rev = "81a58a1c433e6aa89d66211c07e7652407fde1ad";
          sha256 = "sha256-mu3lSppyOULU96lRyKlWq84yksAPEi4W/mxUlEpye5c=";
        };
      }
      {
        name = "thefuck";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-thefuck";
          rev = "6c9a926d045dc404a11854a645917b368f78fc4d";
          sha256 = "sha256-9MbkyEsMsZH+3ct7qJSPvLeLRfVkDEkXRTdg/Rhe0dg=";
        };
      }
    ];

    interactiveShellInit = builtins.readFile ./config.fish;
  };
}
