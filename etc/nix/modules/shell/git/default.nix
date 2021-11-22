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

        aliases = let
          shell = command: "!git ${command}";
          logformat =
            "--pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'";
        in {
          a = "add";
          aa = "add -A";

          b = "branch";
          ba = "branch -a";
          bd = "branch -d";
          bD = "branch -D";
          bnm = "branch --no-merged";
          br = "branch --remote";

          c = "commit -v";
          cm = "commit -m";
          call = "commit -v -a";
          ca = "commit -v --amend";
          cmall = "commit -a -m";
          credo = "commit -v --amend --no-edit";
          credoall = "commit -v -a --amend --no-edit";

          co = "checkout";
          cob = "checkout -b";
          com = shell "checkout $(git_main_branch)";

          cf = "config list";

          cl = "clone --recursive-submodules";
          ccd = shell ''clone --recursive-submodules "$@" && cd "$(basename $_.git)"'';

          d = "diff";
          dc = "diff --cached";
          ds = "diff --staged";
          dup = "diff @{upstream}";

          f = "fetch";
          fa = "fetch --all --prune";
          fo = "fetch origin";
          fu = "fetch upstream";

          pd = "pull";
          pdo = "pull origin";
          pdom = shell "pull origin $(git_default_branch origin)";
          pdu = "pull upstream";
          pdum = shell "pull upstream $(git_default_branch upstream)";

          pu = "push";
          puf = "push --force-with-lease origin";
          puF = "push --force origin";
          puu = shell "push --set-upstream origin $(git_current_branch)";

          l = "log";
          lg = "log --graph ${logformat}";
          lga = "log --graph --all ${logformat}";
          lgo = "log --oneline --decorate --graph";
          lgoa = "log --oneline --decorate --graph --all";

          m = "merge";
          ma = "merge --abort";

          r = "remote";
          rv = "remote -v";
          ra = "remote add";
          rmv = "remote rename";
          rrm = "remote remove";
          rset = "remote set-url";

          rb = "rebase";
          rbi = "rebase -i";
          rbm = shell "rebase $(git_main_branch)";
          rba = "rebase --abort";
          rbc = "rebase --continue";
          rbs = "rebase --skip";
          rbo = "rebase --onto";

          s = "status -sb";
          ss = "status -ss";
          st = "status";

          sh = "stash";
          sha = "stash apply";
          shd = "stash drop";
          shl = "stash list";
          shp = "stash pop";
          shs = "stash show --patch";

          t = "tag";

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
