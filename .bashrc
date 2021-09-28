[ -z "$PS1" ] && return   # If not running interactively, don't do anything

# Surround branch name in square brackets if in a git repo.
# Otherwise do nothing.
git_branch() {
    BRANCH=$(git-branch-name.sh)
    if [[ -n ${BRANCH} ]]; then
        echo "[${BRANCH}]"
    fi
}

dockerb() {
    docker exec -it $1 /bin/bash
}

NUM_COLORS=$(tput colors)

if [ -n NUM_COLORS ]; then
    case "$TERM" in
        xterm* ) export PS1='\[\033[31;1m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[0;31m\]$(git_branch)\[\033[00m\]\$ ' ;;
        *     ) export PS1='\u@\h:\w\$ ' ;;
    esac

    case "$TERM" in
        xterm*|rxct* ) export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"' ;;
    esac
fi

[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"
export HISTCONTROL=ignoredups
shopt -s checkwinsize

alias less="less -R"
alias tree="tree -C"

alias ll="ls -lah"
alias lq="ls -1"
alias la="ls -a1"

alias grpr='grep [Ee][Rr][Rr][Oo][Rr]'
alias con="tail -40 -f /var/log/system.log"

alias em="open -a Emacs"

alias tag="ctags -e -R ."

#
# Init script version managers
#

if [[ -f /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
if [ -n "`which rbenv`" ]; then
    eval "$(rbenv init -)"
fi
if [ -n "`which pyenv`" ]; then
    eval "$(pyenv init -)"
fi
if [ -e "/opt/homebrew/opt/nvm/nvm.sh" ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
fi

export GOPATH=~/code/go
alias goc='go build'
alias got='go test $(go list ./... | grep -v /vendor/)'
alias gotag="find . -type f -name '*.go' | xargs ctags -e"

alias dks='docker stop $(docker ps -q)'
alias dkl='docker logs -f $(docker ps -q)'

alias ispark='PYSPARK_DRIVER_PYTHON=ipython pyspark'

PATH=$PATH:~/bin:~/.binac

#
# System-specific stuff.
#

if [[ $OSTYPE == "darwin"* ]]; then
    man_preview() {
        man -t $1 | open -f -a Preview
    }

    tcplisten() {
        # So there is no more google-ing for
        # https://stackoverflow.com/questions/4421633/who-is-listening-on-a-given-tcp-port-on-mac-os-x
        if [ "$1" -gt "-1" ]; then
           lsof -nP -i4TCP:$1 | grep LISTEN
        else
           lsof -nP -i4TCP | grep LISTEN
        fi
    }

    PATH=/usr/local/smlnj/bin:$PATH

    export CLICOLOR=1
    export JAVA_HOME=/Library/Java/Home
    export BASH_SILENCE_DEPRECATION_WARNING=1

    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    alias code='"/Applications/Visual Studio Code.app//Contents/Resources/app/bin/code"'
    alias mvi='open -a MacVim'
    alias mn=man_preview

    INTELLIJ=`ls -1d /Applications/IntelliJ\ * 2>/dev/null | tail -n1`
    if [ -d "$INTELLIJ" ]; then
        alias intellij="open -a \"$INTELLIJ\""
    fi

    DO_TOKEN=`security find-generic-password -a digital-ocean-token -w 2> /dev/null`
    if ! [ -z $DO_TOKEN ]; then
        # Setup Enviornment for Terraform to access Digital Ocean with account credentials.
        #
        # To setup ed25519 key, see:
        # https://www.digitalocean.com/community/tutorials/how-to-set-up-ssh-keys-2

        export DO_TOKEN

        MD5_FINGERPRINT=`ssh-keygen -E md5 -lf ~/.ssh/id_ed25519.pub | awk '{print $2}'`

        export DO_SSH_FINGERPRINT=${MD5_FINGERPRINT:4}
        export DO_TOKEN=`security find-generic-password -a digital-ocean-token -w`
        export DO_SSH_FINGERPRINT=${MD5_FINGERPRINT:4}
        export DO_PAT=$DO_TOKEN

        export TF_VAR_ssh_fingerprint=${MD5_FINGERPRINT:4}
        export TF_VAR_do_token=$DO_TOKEN
        export TF_VAR_pvt_key=~/.ssh/id_ed25519
        export TF_VAR_pub_key=~/.ssh/id_ed25519.pub
    fi

elif [ "linux-gnu" == $OSTYPE ] ; then

    eval `dircolors -b`

    if [ "$TERM" != "dumb" ]; then
        alias ls='ls --color=auto'
    fi
fi

sourcedir=~/.bashas
if [ -d "$sourcedir" ]; then
    files=`ls -a1 $sourcedir`
    for file in $files; do
        if [ -f "$sourcedir/$file" ]; then
            . "$sourcedir/$file"
        fi
    done
fi

# Read through symbolic link to get actualy directory, then
# source files in .bashas
sourcedir="$( cd "$( dirname "`readlink ${BASH_SOURCE[0]}`" )" && pwd )/.bashas"
if [ -d "$sourcedir" ]; then
    files=`ls -a1 $sourcedir`
    for file in $files; do
        if [ -f "$sourcedir/$file" ]; then
            . "$sourcedir/$file"
        fi
    done
fi
