#! /usr/bin/env bash

BRANCH=${1:-`git rev-parse --abbrev-ref HEAD`}
TRACKING_BRANCH=`git for-each-ref --format='%(upstream:short)' $(git rev-parse --symbolic-full-name ${BRANCH})`

if [ -z "$TRACKING_BRANCH" ]; then
    echo "Could not find a remote tracking branch for $BRANCH"
    exit -1
fi

git fetch . "${TRACKING_BRANCH}":"${BRANCH}"
