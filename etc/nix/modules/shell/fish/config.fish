
#
# Fisher
#

if ! type -q fisher
    echo "Installing fisher..."
    curl -sL https://git.io/fisher | source
    fisher install jorgebucaran/fisher
end

#
# VI mode
#

fish_vi_key_bindings
set -g fish_cursor_default block
set -g fish_cursor_insert line

#
# FZF
#

function fzf-bcd-widget -d 'cd backwards'
    pwd | awk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | tac | eval (__fzfcmd) +m --select-1 --exit-0 $FZF_BCD_OPTS | read -l result
    [ "$result" ]; and cd $result
    commandline -f repaint
end

function fzf-complete -d 'fzf completion and print selection back to commandline'
    # As of 2.6, fish's "complete" function does not understand
    # subcommands. Instead, we use the same hack as __fish_complete_subcommand and
    # extract the subcommand manually.
    set -l cmd (commandline -co) (commandline -ct)
    switch $cmd[1]
        case env sudo
            for i in (seq 2 (count $cmd))
                switch $cmd[$i]
                    case '-*'
                    case '*=*'
                    case '*'
                        set cmd $cmd[$i..-1]
                        break
                end
            end
    end
    set cmd (string join -- ' ' $cmd)

    set -l complist (complete -C$cmd)
    set -l result
    string join -- \n $complist | sort | eval (__fzfcmd) -m --select-1 --exit-0 --header '(commandline)' | cut -f1 | while read -l r
        set result $result $r
    end

    set prefix (string sub -s 1 -l 1 -- (commandline -t))
    for i in (seq (count $result))
        set -l r $result[$i]
        switch $prefix
            case "'"
                commandline -t -- (string escape -- $r)
            case '"'
                if string match '*"*' -- $r >/dev/null
                    commandline -t -- (string escape -- $r)
                else
                    commandline -t -- '"'$r'"'
                end
            case '~'
                commandline -t -- (string sub -s 2 (string escape -n -- $r))
            case '*'
                commandline -t -- (string escape -n -- $r)
        end
        [ $i -lt (count $result) ]; and commandline -i ' '
    end

    commandline -f repaint
end

function snag -d "Pick desired files from a chosen branch"
    # use fzf to choose source branch to snag files FROM
    set branch (git for-each-ref --format='%(refname:short)' refs/heads | fzf --height 20% --layout=reverse --border)
    # avoid doing work if branch isn't set
    if test -n "$branch"
        # use fzf to choose files that differ from current branch
        set files (git diff --name-only $branch | fzf --height 20% --layout=reverse --border --multi)
    end
    # avoid checking out branch if files aren't specified
    if test -n "$files"
        git checkout $branch $files
    end
end

function fssh -d "Fuzzy-find ssh host via ag and ssh into it"
    rg --ignore-case '^host [^*]' ~/.ssh/config | cut -d ' ' -f 2 | fzf | read -l result; and ssh "$result"
end

for mode in default insert
    bind -M $mode \eC fzf-bcd-widget
    bind -M $mode \t fzf-complete
end
