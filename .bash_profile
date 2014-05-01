# ~/.bash_profile: executed by bash(1) for login shells.

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

if [ -d ~/bin ] ; then
    export PATH=~/bin:"${PATH}"
fi
