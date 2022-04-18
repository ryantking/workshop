{ config, pkgs, lib, ... }:

let
  inherit (config) whoami;
in
{
  shell.env = {
    GITHUB_USER = whoami.usernames.github;
    GITHUB_NAME = whoami.name;
    GITHUB_EMAIL = whoami.emails.personal;
  };

  programs = {
    git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      userName = whoami.name;
      userEmail = whoami.emails.personal;

      signing = {
        key = whoami.keys.pgp;
        signByDefault = true;
      };

      extraConfig = {
        init.defaultBranch = "master";
        pull.rebase = false;
        tag.sort = "version:refname";
        github.user = whoami.usernames.github;
      };

      aliases = {
        pristine = "!git resest --hard && git clean -dffx";
        whohas = "branch -a --contains";
        ignore =
          "!gi() { curl -sL https://www.toptal.com/developers/gitignore/api/$@ ;}; gi";
        fixbare = ''
          config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"'';
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
        ".envrc"
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
    packages = with pkgs; [
      git-crypt
      git-up
      git-my
      git-open
      git-trim
      git-subrepo
      git-standup
    ];

    sessionVariables = { DELTA_PAGER = "less -FR"; };
  };

  age.secrets."gh-hosts" = {
    file = ./hosts.yml.age;
    path = "${config.xdg.configHome}/gh/hosts.yml";
  };
}
