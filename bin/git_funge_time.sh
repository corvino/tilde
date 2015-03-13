#!/usr/bin/env bash

# Usage: git_funge_time.sh <git-hash> <date>
#
# Changes the author and commiter date of the specified commit to the
# specified date.
#
# example:
#     git_funge_time.sh 4580cb12470e4ef85394dc0f1a2af5290b9e6357 \
#                       'Sun Mar 2 09:22:25 2015 -0600'
#

if [ -z $1 ]; then
    echo "You must specify a commit hash"
    exit -1
fi

if [ -z $2 ]; then
    echo "You must specify a commit date"
    exit -1
fi

git filter-branch --env-filter \
    "if [ \$GIT_COMMIT = \"$1\" ]
     then
         export GIT_AUTHOR_DATE=\"$2\"
         export GIT_COMMITTER_DATE=\"$2\"
     fi"
