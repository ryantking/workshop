#!/usr/bin/env bash

icon=""
mem="$(free --mebi | sed -n '2{p;q}' | awk '{printf ("%d", ($3 / $2 * 100))}')"

echo -e " $icon $mem%"

# memory.sh ends here
