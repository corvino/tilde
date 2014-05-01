[ -z "$PS1" ] && return   # If not running interactively, don't do anything

case "$TERM" in
    xterm* ) export PS1='\[\033[31;1m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ ' ;;
    *     ) export PS1='\u@\h:\w\$ ' ;;
esac

case "$TERM" in
    xterm*|rxct* ) export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"' ;;
esac

[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"
export HISTCONTROL=ignoredups
shopt -s checkwinsize

#
# System-specific stuff.
#

if [[ $OSTYPE == "darwin"* ]]
then

    export CLICOLOR=1
    export JAVA_HOME=/Library/Java/Home

    alias em='open -a Emacs'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'

elif [ "linux-gnu" == $OSTYPE ] ; then

    eval `dircolors -b`

    if [ "$TERM" != "dumb" ]; then
        alias ls='ls --color=auto'
    fi
fi
