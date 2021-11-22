{ pkgs, ... }:

{
  hm = {
    programs = {
      git = {
        enable = true;
        package = pkgs.gitAndTools.gitFull;
        userName = "Ryan King";
        userEmail = "ryantking@protonmail.com";

        signing = {
          key = "0xD718BA353C298BB2";
          signByDefault = true;
        };

        extraConfig = {
          init.defaultBranch = "master";
          pull.rebase = false;
          tag.sort = "version:refname";
        };

        aliases = {
          pristine = "!git resest --hard && git clean -dffx";
          whohas = "branch -a --contains";
          ignore = "!gi() { curl -sL https://www.toptal.com/developers/gitignore/api/$@ ;}; gi";
          fixbare = ''config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"'';
        };

        ignores = [
          "*~"
          ".fuse_hidden*"
          ".directory"
          ".Trash-*"
          ".nfs*"
          ".DS_Store"
          ".AppleDouble"
          ".LSOverride"
          "Icon"
          "._*"
          ".DocumentRevisions-V100"
          ".fseventsd"
          ".Spotlight-V100"
          ".TemporaryItems"
          ".Trashes"
          ".VolumeIcon.icns"
          ".com.apple.timemachine.donotpresent"
          ".AppleDB"
          ".AppleDesktop"
          "Network Trash Folder"
          "Temporary Items"
          ".apdisk"
          "[._]*.s[a-v][a-z]"
          "[._]*.sw[a-p]"
          "[._]s[a-rt-v][a-z]"
          "[._]ss[a-gi-z]"
          "[._]sw[a-p]"
          "Session.vim"
          "Sessionx.vim"
          ".netrwhist"
          "tags"
          "[._]*.un~"
        ];

        delta = {
          enable = true;

          options = {
            line-numbers = true;
            features = "decorations";
            decorations = {
              file-style = "bold white ul";
              file-decoration-stlye = "none";
              whitespace-error-style = "22 reverse";
            };
          };
        };
      };

      gh = {
        enable = true;

        settings = {
          gitProtocol = "ssh";

          aliases = {
            co = "pr checkout";
            cl = "repo clone";
          };
        };
      };
    };

    home = {
      packages = with pkgs;
        let
          gitCurrentBranch =
            writeShellScriptBin "git_current_branch" "git rev-parse --abbrev-ref HEAD";

          gitMainBranch = writeShellScriptBin "git_main_branch" ''
            command git rev-parse --git-dir &>/dev/null || exit
            for ref in refs/{heads,remotes/{origin,upstream}}/{main,trunk}; do
              if command git show-ref -q --verify $ref; then
                echo $(basename $ref)
                exit
              fi
            done

            echo master
          '';

          gitDefaultBranch = writeShellScriptBin "git_default_branch"
            "git remote show $1 | grep \"HEAD branch\" | sed 's/.*: //'";
        in [
          git-crypt
          git-up
          git-my
          git-open
          git-trim
          git-subrepo
          git-standup

          gitCurrentBranch
          gitMainBranch
          gitDefaultBranch
        ];

      file.".config/gh/hosts.yml".source = ./hosts.yml;

      sessionVariables = { DELTA_PAGER = "less -FR"; };
    };
  };
}
