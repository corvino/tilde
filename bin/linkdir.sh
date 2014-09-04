#!/usr/bin/env bash

# This script allows easy symbolic linking of ALL files in one directory
# to another directory.

if [ -z $1 ]; then
    echo "You must specify a source location"
    exit -1
fi

if [ -z $2 ]; then
    echo "You must specify a target location"
    exit -1
fi

sourcedir="$1"
targetdir="$2"
files=`ls -1 $sourcedir`

for file in $files; do
    source="$sourcedir/$file"
    target="$targetdir/$file"

    if [[ -e $target || -L $target ]]; then
        if [[ -L $target && $target == `readlink $target` ]]; then
            echo "skipped $file because it is already properly linked"
        else
            # This could be a forced relink; but that should be an
            # option, and not the default.
            echo "skipped $file because it is linked to `readlink $target`"
        fi
    else
        ln -s $source $target
        echo "linked $file"
    fi
done
