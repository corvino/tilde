#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
olddir="$dir/dotfiles.OLD"

files=".bash_profile .bashrc .emacs .elisp .gitconfig .gitexclude .vimrc .lldbinit .lldb"

# Create the "backup" directory for existing files; but only if there
# are existing files that aren't already symlinked appropriately. Fail
# if a backup is called for but one already exists. There is no
# "merging" of backups!

for file in $files; do
    if [[ -e ~/$file && ! (-L ~/$file && $dir/$file == `readlink ~/$file`) ]]; then
        if [ -e $olddir ]; then
            echo "$olddir already exists; can't move files out of the way."
            exit -1
        else
            echo "Create $olddir"
            mkdir -p $olddir
            break
        fi
    fi
done

# Run through the files, moving and symlinking as necessary. If nothing
# needs to be done, will simply output a list of the files checked.

echo "Move existing files to $olddir and symlink from ~ to $dir (as necessary):"

for file in $files; do
    echo -n "  "
    if [ -e ~/$file ]; then
        if [[ -L ~/$file && $dir/$file == `readlink ~/$file` ]]; then
            # All done!!
            echo -n "   "
        else
            echo -n "m+l"
            mv ~/$file $olddir/
            ln -s $dir/$file ~/$file
        fi
    else
        echo -n "  l"
        ln -s $dir/$file ~/$file
    fi
    echo "  $file"
done
