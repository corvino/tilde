#! /usr/bin/env bash

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

repo_info() {
    cd "$1"
    BRANCH=$(parse_git_branch)
    echo $(dirname "$1"):$BRANCH:$(git rev-parse HEAD)
}

export -f parse_git_branch
export -f repo_info

find . -name '.git' -exec bash -c 'repo_info "$0"' {}  \; | column -t -s':' | sort
