#! /usr/bin/env bash

export SOURCE_DIR=$(dirname "$0")

ff() {
    pushd "$1" > /dev/null

    echo \* $1\:
    STARTING_COMMIT=$(git rev-parse HEAD)

    git fetch
    git merge --ff-only

    ENDING_COMMIT=$(git rev-parse HEAD)

    echo \* $STARTING_COMMIT -\> $ENDING_COMMIT

    popd > /dev/null
}

export -f ff

${SOURCE_DIR}/git-sub-stats.sh -n -o -p \
    | awk '
        BEGIN { FS = ":" }
        1 != NR { print "" }
        {
            system("ff "$1)
        }'
