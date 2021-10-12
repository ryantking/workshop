{ cfg, pkg }:

let
  logformat = "--format=format:'%C(blue)%h%C(reset) - %C(green)(%ar)%C(reset) %s %C(italic)- %an%C(reset)%C(magenta bold)%d%C(reset)'";
  defaultBranch = remote: ''!b() { git remote show ${remote} | grep "HEAD branch" | sed 's/.*: //' ;};'';
in {
  enable = true;
  package = pkg;

  inherit (cfg) userName userEmail;

  signing = {
    key = "0xD718BA353C298BB2";
    signByDefault = true;
  };

  aliases = {
    a = "add";
    aa = "add -A";
    d = "diff";
    dc = "diff --cached";
    st = "status";

    c = "commit";
    ca = "commit --amend --verbose";
    call = "commit --all";
    cm = "commit --message";
    credo = "commit --amend --no-edit";
    credoall = "commit --amend --no-edit --all";

    co = "checkout";
    cob = "checkout -b";
    com = "${defaultBranch "origin"} git checkout $(b)";

    l = "log";
    lg = "log --oneline --graph --decorate ${logformat}";
    lga = "log --oneline --graph --decorate --all ${logformat}";

    f = "fetch";
    pd = "pull";
    pdu = "pull upstream";
    pdum = "${defaultBranch "upstream"} git pull upstream $(b)";
    pu = "push";
    puf = "push --force-with-lease";
    puu = "!head() { git rev-parse --abbrev-ref HEAD ;}; git push --set-upstream origin $(head)";

    rb = "rebase";
    rbi = "rebase --interactive"; 
    rba = "rebase --abort";
    rbc = "rebase --continue";

    sh = "stash";
    sha = "stash apply";
    shd = "stash drop";
    shl = "stash list";
    shp = "stash pop";
    shs = "stash show --patch";

    whohas = "branch -a --contains";
    ignore = "!gi() { curl -sL https://www.toptal.com/developers/gitignore/api/$@ ;}; gi";
    fixbare = "config remote.origin.fetch \"+refs/heads/*:refs/remotes/origin/*\"";
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

  extraConfig = {
    init.defaultBranch = "master";
    pull.rebase = false;
    tag.sort = "version:refname";
  };
}
