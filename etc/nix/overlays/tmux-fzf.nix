final: prev: {
  tmuxPlugins = prev.tmuxPlugins // {
    tmux-fzf = prev.tmuxPlugins.tmux-fzf.overrideAttrs (old: {
      src = prev.fetchFromGitHub {
        owner = "sainnhe";
        repo = "tmux-fzf";
        rev = "8bba6a429d3ec808031f972384f0bd28b0ce38c1";
        sha256 = "sha256-QbBqv1cFSuGR97layXoeBpd5MP6PGlRhmLbRd6KYh6Y=";
      };
    });
  };
}
