#!/usr/bin/env bash

# Usage: unlinkdir.sh <source-directory> <target-directory>
#
# This script removes all symbolic links in the source directory than
# link to the a file in the target directory.
#
# For example, the MacTex (https://tug.org/mactex/) binaries may have be
# linked to /usr/local/bin using the command:
#
#     ln -s /usr/local/texlive/2014/bin/x86_64-darwin/* /usr/local/bin/
#
# When a new version of MacTex is downloaded, the installation directory
# will be different. To remove the old links, use:
#
#     unlinkdir.sh /usr/local/bin/ /usr/local/texlive/2014/bin/x86_64-darwin/
#
# Note that the source specification is a directory, not a list of files.
#

if [ -z $1 ]; then
    echo "You must specify a source location"
    exit -1
fi

if [ -z $2 ]; then
    echo "You must specify a target location"
    exit -1
fi

sourcedir="$1"
# Strip trailing slashes from $2
targetdir=`echo "$2" | sed -e 's#/*$##'`

files=`ls -a1 $sourcedir`

for file in $files; do
    sourcefile="$sourcedir/$file"
    if [[ -L $sourcefile ]]; then
        sourcetarget=`readlink "$sourcefile"`
        targeteddir=$(dirname "$sourcetarget")

        if [ "$targeteddir" = "$targetdir" ]; then
            rm "$sourcefile"
        fi
    fi
done
