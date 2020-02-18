#! /usr/bin/env bash

sync_branch() {
    pushd "$1" > /dev/null

    echo \* $1 -\> $2:
    git fetch
    git checkout "$2"
    git merge

    popd > /dev/null
}

export -f sync_branch

# Input expected from git-sub-stats.sh -o -n -p

sed '/^#/ d' | awk '
    BEGIN { FS = ":" }
    1 != NR { print "" }
    { system("sync_branch " $1 " " $2) }'
