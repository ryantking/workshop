#!/usr/bin/env bash

set -euo pipefail

sed -n '/START_KEYS/,/END_KEYS/p' ~/.config/xmonad/lib/XMonad/Custom/Keys.hs \
    | grep -e ', ("' -e '\[ ("' -e 'KB_GROUP' \
    | grep -v '\-\- , ("' \
    | sed \
	  -e 's/^[ \t]*//' \
	  -e 's/, (/(/' \
	  -e 's/\[ (/(/' \
	  -e 's/\-\- KB_GROUP /\n/' \
	  -e 's/", /"\t- /' \
	  -e 's/myTerminal ++ "/"kitty/' \
    | yad --text-info --back=#2E3440 --fore=#D8DEE9 --geometry=1200x800
