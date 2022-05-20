{
  mkdir = "mkdir -pv";

  "reload!" = "exec zsh";

  l = "exa";
  ls = "exa --color always --group-directories-first --git";
  ll = "exa --classify --group --icons --oneline --long";
  la = "exa --all --all --classify --extended --header --long";
  ld = "exa --oneline --only-dirs";
  lld = "exa --all --classify --git --group --group-directories-first --header --long";
  exa = "exa --color always --group-directories-first --git";
  tree = "exa --tree";

  flushdns = "dscacheutil -flushcache";
  emptytrash = "sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl";
}
