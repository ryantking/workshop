{ config, lib, pkgs, ... }:

# TODO: Switch over to zplug for lazy loading shit
# TODO: Refactor omz and plugins to new files.
# TODO: Fix history substring search
# TODO: Fix FZF styling
# TODO: Move prompt to root prompt (currently starship)
# TODO: Get tmux operational.
# TODO: Checkout presto, cross check plugins I use.
# TODO: Use zplug for omz/presto?

{
  home.packages = with pkgs; [ zoxide ];

  programs = {
    fzf = let
      fdCommand = "fd --follow --hidden --exclude .git";
      fzfOpts = with config.colorscheme.colors;
      # "--no-bold --prompt='/ ' --pointer=• --marker=• --color " +
        "--no-bold --prompt='/ ' --color "
        + "fg:#${base06},bg:-1,hl:#${base0C},fg+:#${base04},bg+:-1,hl+:#${base0B},"
        + "pointer:#${base0D},info:#${base03},spinner:#${base03},header:#${base03},prompt:#${base03},marker:#${base0D}";
    in {
      enable = true;
      enableZshIntegration = true;

      defaultCommand = "${fdCommand}";
      defaultOptions = [ "${fzfOpts}" ];
      fileWidgetCommand = "${fdCommand}";
      fileWidgetOptions = [ "${fzfOpts} --preview 'bat --color=always --plain {}'" ];
      changeDirWidgetCommand = "${fdCommand} --type d";
      changeDirWidgetOptions =
        [ "${fzfOpts} --preview 'exa -l --tree --level=2 --color=always {}'" ];
    };

    zsh = {
      enable = true;
      dotDir = ".config/zsh";
      autocd = true;
      enableCompletion = true;
      enableAutosuggestions = true;
      enableVteIntegration = true;

      history = {
        path = config.programs.zsh.dotDir + "/.zsh_history";
        share = true;
        save = 10000000;
        ignoreDups = true;
        extended = true;
        size = 10000000;
      };
      
      localVariables = {
        FZF_BASE = "${pkgs.fzf}/share/fzf";
        PURE_PROMPT_SYMBOL = "➜";
        PURE_PROMPT_VICMD_SYMBOL = "";
      };

      zplug = {
      	enable = true;

        plugins = [
          { name = "chrissicool/zsh-256color"; }
          { name = "mafredri/zsh-async"; }
          { name = "sindresorhus/pure" ; tags = [ "use:pure.zsh" "as:theme" ]; }
          { name = "jeffreytse/zsh-vi-mode"; }
          { name = "zsh-users/zsh-history-substring-search"; }
          { name = "zdharma-continuum/fast-syntax-highlighting"; }
          { name = "Aloxaf/fzf-tab"; }
          { name = "Tarrasch/zsh-bd"; }
          { name = "chisui/zsh-nix-shell"; }
        ];
      };

      oh-my-zsh = {
        enable = true;
        plugins = [
          "aliases"
          "alias-finder"
          "cargo"
          "colored-man-pages"
          "copydir"
          "copyfile"
          "cp"
          "direnv"
          "dirhistory"
          "docker"
          "extract"
          "fancy-ctrl-z"
          "fd"
          "fzf"
          "gh"
          "git"
          "golang"
          "gpg-agent"
          "grc"
          "history-substring-search"
          "ripgrep"
          "rustup"
          "safe-paste"
          "sudo"
          "systemd"
          "zoxide"
        ];
      };

      initExtraFirst = with pkgs; ''
setopt HIST_IGNORE_ALL_DUPS
setopt no_extendedglob

# Fix plugins that need bindings as vi-mode nukes them.
function zvm_before_init() {
  zvm_bindkey viins '^[[A' history-beginning-search-backward
  zvm_bindkey viins '^[[B' history-beginning-search-forward
  zvm_bindkey vicmd '^[[A' history-beginning-search-backward
  zvm_bindkey vicmd '^[[B' history-beginning-search-forward
}
function zvm_after_init() {
  source ${oh-my-zsh}/share/oh-my-zsh/plugins/fzf/fzf.plugin.zsh
  source ${oh-my-zsh}/share/oh-my-zsh/plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh
}
      '';
    };
  };
}
