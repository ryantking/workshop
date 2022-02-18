# --------------------------------------
# Plugins
# --------------------------------------

# Load Zinit
source "${ZDOTDIR}/zinit.zsh"

# Load vi-mode first so its bindings can be overwritten
zinit depth'1' light-mode for jeffreytse/zsh-vi-mode

# Pure Prompt
zinit pick"async.zsh" src"pure.zsh" light-mode atload'
  export PURE_PROMPT_SYMBOL="➜"
  export PURE_PROMPT_VICMD_SYMBOL=""
' for sindresorhus/pure

# Basic Plugins
zinit lucid depth'1' wait'0a' light-mode for \
    silent atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" zdharma-continuum/fast-syntax-highlighting \
    atload"!_zsh_autosuggest_start" zsh-users/zsh-autosuggestions \
    as"completion" zsh-users/zsh-completions \
    pick'autopair.zsh' nocompletions atload'autopair-init' hlissner/zsh-autopair \
    tarrasch/zsh-bd \
    chisui/zsh-nix-shell \
    pick'.kubectl_aliases' ahmetb/kubectl-aliases \
    atload'!
        export HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND="fg=green,bold"
        export HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND="fg=red,bold"

        bindkey   "$terminfo[kcuu1]"  history-substring-search-up;
        bindkey   "^[[A"              history-substring-search-up;
        bindkey   -M vicmd "^[[A"     history-substring-search-up;
        bindkey   "^[[B"              history-substring-search-down;
        bindkey   -M vicmd "^[[B"     history-substring-search-down;
        bindkey   "$terminfo[kcud1]"  history-substring-search-down;
    ' zsh-users/zsh-history-substring-search

# FZF Plugins
zinit lucid depth'1' wait'0a' light-mode for \
    Aloxaf/fzf-tab \
    wfxr/forgit

# Completion plugins
zinit lucid depth'1' wait'0b' from'gh-r' as'program' for \
    sei40kr/fast-alias-tips-bin \
    sei40kr/zsh-fast-alias-tips

zinit lucid depth'1' wait'2a' for \
    dim-an/cod \
    atload'zstyle :plugin:zsh-completion-generator programs grep' RobSis/zsh-completion-generator

# OMZ Plugins
zinit wait lucid atload"zicompinit; zicdreplay" as"completion" blockf for \
    OMZP::docker/_docker \
    OMZP::fd/_fd \
    OMZP::ripgrep/_ripgrep

zinit lucid wait light-mode for \
    OMZP::aliases \
    OMZP::alias-finder \
    OMZP::colored-man-pages \
    OMZP::copydir \
    OMZP::copyfile \
    OMZP::cp \
    OMZP::direnv \
    OMZP::dirhistory \
    OMZP::extract \
    OMZP::fancy-ctrl-z \
    OMZP::fzf \
    OMZP::gh \
    OMZP::git \
    OMZP::golang \
    OMZP::gpg-agent \
    OMZP::grc \
    OMZP::helm \
    OMZP::jsontools \
    OMZP::oc \
    OMZP::operator-sdk \
    OMZP::rust \
    OMZP::safe-paste \
    OMZP::sudo \
    OMZP::tmux \
    OMZP::zoxide
