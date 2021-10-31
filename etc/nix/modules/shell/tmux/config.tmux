# -----------------------------------------------------------------------------
# General settings
# -----------------------------------------------------------------------------

# Mouse
set -g mouse on

# Improve colors
set-option -ga terminal-overrides ",xterm-256color:Tc"

# Renumber windows
set -g renumber-windows on

# -----------------------------------------------------------------------------
# Key bindings
# -----------------------------------------------------------------------------

# Cycle through panes
unbind C-a
bind C-a select-pane -t :.+

# Custom bindings
bind n next-window # Cycle through windows
bind R source-file ~/.tmux.conf # Reload settings
bind-key d detach # Detach client
bind-key A command-prompt "rename-window %%" # Rename window
bind-key -T copy-mode-vi v send-keys -X begin-selection # setup 'v' begin/end selection (match vim)
bind c new-window -c "#{pane_current_path}"

# Split windows
bind-key - split-window -v
bind-key | split-window -h

# bind < and > to swap windows
bind-key -r < swap-window -t -1
bind-key -r > swap-window -t +1

# Copy/paste mode
bind-key Escape copy-mode
bind-key [ copy-mode

# Neovim integration
is_vim="ps -o state= -o comm= -t '#{pane_tty}' | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|n?vim?x?)(diff)?$'"

bind-key -n 'C-Left' if-shell "$is_vim" 'send-keys C-Left' { if -F '#{pane_at_left}' '' 'select-pane -L' }
bind-key -n 'C-Down' if-shell "$is_vim" 'send-keys C-Down' { if -F '#{pane_at_bottom}' '' 'select-pane -D' }
bind-key -n 'C-Up' if-shell "$is_vim" 'send-keys C-Up' { if -F '#{pane_at_top}' '' 'select-pane -U' }
bind-key -n 'C-Right' if-shell "$is_vim" 'send-keys C-Right' { if -F '#{pane_at_right}' '' 'select-pane -R' }

bind-key -T copy-mode-vi 'C-Left' if -F '#{pane_at_left}' '' 'select-pane -L'
bind-key -T copy-mode-vi 'C-Down' if -F '#{pane_at_bottom}' '' 'select-pane -D'
bind-key -T copy-mode-vi 'C-Up' if -F '#{pane_at_top}' '' 'select-pane -U'
bind-key -T copy-mode-vi 'C-Right' if -F '#{pane_at_right}' '' 'select-pane -R'

bind -n 'M-Left' if-shell "$is_vim" 'send-keys M-Left' 'resize-pane -L 1'
bind -n 'M-Down' if-shell "$is_vim" 'send-keys M-Down' 'resize-pane -D 1'
bind -n 'M-Up' if-shell "$is_vim" 'send-keys M-Up' 'resize-pane -U 1'
bind -n 'M-Right' if-shell "$is_vim" 'send-keys M-Right' 'resize-pane -R 1'

bind-key -T copy-mode-vi M-Left resize-pane -L 1
bind-key -T copy-mode-vi M-Down resize-pane -D 1
bind-key -T copy-mode-vi M-Up resize-pane -U 1
bind-key -T copy-mode-vi M-Right resize-pane -R 1

# -----------------------------------------------------------------------------
# Status Bar
# -----------------------------------------------------------------------------

set-option -g status-style fg=default,bg=default
set -g status-position bottom

set -g status-left "#S"
set -g status-justify centre
setw -g window-status-format ' #(echo "#{pane_current_command}") '
setw -g window-status-current-format ' #(echo "#{pane_current_command}") '
set -g status-right "%b %d %R"

setw -g window-status-style fg=default,bg=default
setw -g window-status-current-style fg=cyan,bold
setw -g window-status-activity-style fg=red,bright

# -----------------------------------------------------------------------------
# Colors
# -----------------------------------------------------------------------------

set -g pane-border-style bg=default,fg=brightblack
set -g pane-active-border-style bg=default,fg=blue

set -g display-panes-colour black
set -g display-panes-active-colour brightblack

setw -g clock-mode-colour cyan
set -g mode-style fg=black,bg=cyan

set -g message-style bg=brightblack,fg=cyan
set -g message-command-style bg=brightblack,fg=cyan
