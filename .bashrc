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

alias ll="ls -lah"
alias lq="ls -1"
alias la="ls -a1"

alias grpr='grep [Ee][Rr][Rr][Oo][Rr]'
alias con="tail -40 -f /var/log/system.log"

alias tag="ctags -e -R ."

alias rbi='eval "$(rbenv init -)"'
alias pyi='eval "$(pyenv init -)"'

export GOPATH=~/code/go
alias goc='go build'
alias got='go test $(go list ./... | grep -v /vendor/)'
alias gotag="find . -type f -name '*.go' | xargs ctags -e"

alias dks='docker stop $(docker ps -q)'
alias dkl='docker logs -f $(docker ps -q)'

alias ispark='PYSPARK_DRIVER_PYTHON=ipython pyspark'

PATH=$PATH:~/.binac

#
# System-specific stuff.
#

if [[ $OSTYPE == "darwin"* ]]; then
    man_preview() {
        man -t $1 | open -f -a Preview
    }

    export CLICOLOR=1
    export JAVA_HOME=/Library/Java/Home

    alias em='open -a Emacs'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    alias mvi='open -a MacVim'
    alias mn=man_preview

    INTELLIJ=`ls -1d /Applications/IntelliJ\ * | tail -n1`
    if [ -d "$INTELLIJ" ]; then
        alias intellij="open -a \"$INTELLIJ\""
    fi
    GOGLAND=`ls -1d /Applications/Gogland\ * | tail -n1`
    if [ -d "$GOGLAND" ]; then
        alias gogland="open -a \"$GOGLAND\""
    fi

elif [ "linux-gnu" == $OSTYPE ] ; then

    eval `dircolors -b`

    if [ "$TERM" != "dumb" ]; then
        alias ls='ls --color=auto'
    fi
fi

sourcedir=~/.bashac
if [ -d "$sourcedir" ]; then
    files=`ls -a1 $sourcedir`
    for file in $files; do
        if [ -f "$sourcedir/$file" ]; then
            . "$sourcedir/$file"
        fi
    done
fi
