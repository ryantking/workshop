# -*- mode: sh[ -*-
# shellcheck disable=all

# dtach.zsh: dtach session manager for ZSH


# Returns a default session name comprised of the shell PID,
# TTY, and hostname.
function dtach-default-session-name {
    local wd="$(pwd)"
    local tty="${TTY#/dev/}"
    tty="${name//\//-}"
    print "$$.$tty.$(hostname)/$(basename wd)"
}

# Lists all sessions in the dtach socket directory.
# Returns 1 if no sessions exist.
function dtach-list {
  local socket_dir="${TMP_DIR:-/tmp}/dtach.$(whoami)"
  [[ ! -d "$socket_dir" ]] && return

  local sockets=("$socket_dir/"*.socket(N))
  if [[ "${#sockets[@]}" == 0 ]]; then
print "No sessions found in $socket_dir"
return 1
  fi

  print "Sessions:"
  for socket in "$socket_dir/"*.socket(N); do
	  local state="Detached"
	  [[ "$DTACH_SOCKET" = "$socket" ]] && state="Attached"
	  socket="${socket##*/}"
	  socket="${socket%.*}"
	  print "\t$socket ($state)"
  done
  print "${#sockets[@]} sessions in $socket_dir"
}

# Attaches to a session in the dtach socket directory.
# The session is created if it does not exist.
function dtach-attach {
    local name="$(dtach-default-session-name)"
    local cmd="$SHELL"
    local socket_dir="${TMP_DIR:-/tmp}/dtach.$(whoami)"

    [[ $# > 0 ]] && name="$1" && shift
    [[ $# > 1 ]] && cmd="$2"  && shift

    export DTACH=1
    export DTACH_SOCKET="$socket_dir/$name.socket"

    echo -en "\e]2;dtach: $name\a"
    [[ ! -d "$socket_dir" ]] && mkdir -p "$socket_dir"
    dtach -A "$socket_dir/$name.socket" "$@" "$cmd"

    unset DTACH
    unset DTACH_SOCKET
}

# A DWIM function that creates a session if a name is
# provided, otherwise lists sefssions.
function dtach-dwim {
    if [[ $# == 0 ]]; then
	dtach-list
	return
    fi

    if [ ! -z "$session" ]; then
	BUFFER="dtach-attach $session"
	zle accept-line
    fi

    screen -r $session
}

# A widget for selecting a dtach session.
function dtach-session-widget () {
    emulate -L zsh
    zle -I

    local session=$(screen -ls \
			| head -n -1 \
			| tail -n +2 \
			| awk '{ print $1, $2 }' \
			| fzy -p "socket>>" \
			| awk '{ print $1 }')

    if [ ! -z "$session" ]; then
	BUFFER="screen -r $session"
	zle accept-line
    else
	zle reset-prompt
    fi
   
}

zle -N dtach-session-widget

# dtach.zsh ends here
