# --------------------------------------
# Plugins
# --------------------------------------

# Load Zinit
source "${ZINIT_HOME}/zinit.zsh"

# Load colors
source "${XDG_CONFIG_DIR:-$HOME/.config}/colorrc"

# Load vi-mode first so its bindings can be overwritten
zinit depth'1' if'[[ $TERM != "dumb" && -z $INSIDE_EMACS ]]' light-mode for jeffreytse/zsh-vi-mode

# Basic Plugins
zinit lucid depth'1' wait'0a' light-mode for \
    silent atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" zdharma-continuum/fast-syntax-highlighting \
    atload'!_zsh_autosuggest_start
        export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#$COLOR_BG_ALT"
    ' zsh-users/zsh-autosuggestions \
    as"completion" zsh-users/zsh-completions \
    as"completion" spwhitt/nix-zsh-completions \
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
    OMZP::copypath \
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
    OMZP::zoxide
