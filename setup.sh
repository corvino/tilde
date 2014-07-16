#!/bin/bash

dir=~/.dotfiles
olddir=~/dotfiles.old

files=".bash_profile .bashrc .emacs .elisp .gitconfig .gitexclude"

if [ -e $olddir ]; then
    echo "$olddir already exists; can't move files out of the way."
    exit -1
fi

echo "Create $olddir"
mkdir -p $olddir

echo "Move existing files to $olddir and symlink from ~ to $dir:"

for file in $files; do
    echo -n "  "
    if [ -e ~/$file ]; then
        echo -n "m+"
        mv ~/$file $olddir/
    else
        echo -n " "
    fi

    ln -s $dir/$file ~/$file
    echo "l  $file"
done
