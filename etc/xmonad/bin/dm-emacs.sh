#!/usr/bin/env sh

set -euo pipefail

main() {
    dir="$HOME/.config/emacs/ryan-lisp"
    files=$(/bin/ls "$dir"/*.el)
    action=$(echo "$files" | awk -F '/' '{print $NF}' | dmenu -p 'Edit')
    emacsclient --socket-name "/tmp/emacs$UID/server" -c -a emacs "$dir/$action"
}

[[ "${BASH_SOURCE[0]}" == "${0}" ]] && main "$@"
