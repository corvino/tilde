[ -z "$PS1" ] && return   # If not running interactively, don't do anything

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/[\1]/'
}

case "$TERM" in
#    xterm* ) export PS1='\[\033[31;1m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ ' ;;
    xterm* ) export PS1='\[\033[31;1m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[0;31m\]$(parse_git_branch)\[\033[00m\]\$ ' ;;
    *     ) export PS1='\u@\h:\w\$ ' ;;
esac

case "$TERM" in
    xterm*|rxct* ) export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"' ;;
esac

[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"
export HISTCONTROL=ignoredups
shopt -s checkwinsize

alias rbi='eval "$(rbenv init -)"'
alias pyi='eval "$(pyenv init -)"'

#
# System-specific stuff.
#

if [[ $OSTYPE == "darwin"* ]]
then
    man_preview() {
        man -t $1 | open -f -a Preview
    }

    export CLICOLOR=1
    export JAVA_HOME=/Library/Java/Home
    export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/bin

    alias em='open -a Emacs'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    alias mvi='open -a MacVim'
    alias mn=man_preview

elif [ "linux-gnu" == $OSTYPE ] ; then

    eval `dircolors -b`

    if [ "$TERM" != "dumb" ]; then
        alias ls='ls --color=auto'
    fi
fi
