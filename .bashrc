SOURCE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

[ -z "$PS1" ] && return   # If not running interactively, don't do anything

# Surround branch name in square brackets if in a git repo.
# Otherwise do nothing.
git_branch() {
    BRANCH=$(git-branch-name.sh)
    if [[ -n ${BRANCH} ]]; then
        echo "[${BRANCH}]"
    fi
}

command_exists() {
    return $(command -v "$1" &> /dev/null)
}

path_contains() {
    if [[ $PATH =~ ^"$1":|:"$1":|:"$1"$ ]]; then
        return $(true)
    else
        return $(false)
    fi
}

augment_path() {
    IFS=":"
    read -a dirs <<< "$1"
    IFS=""

    for dir in "${dirs[@]}"
    do
        if ! $(path_contains "$dir"); then
            PATH="$dir:$PATH"
        fi
    done
}

NUM_COLORS=$(tput colors)

function prompt {
    source "${SOURCE_DIR}/.bash_lib/colors.bash"
    if [ -n NUM_COLORS ]; then
        case "$TERM" in
            xterm* ) echo "${a_red}\u@\h${df_clr}:${a_blue}\w${a_red}\$(git_branch)${df_clr}\$" ;;
            *) echo ""
        esac
    fi
}
PS1=$(prompt)

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

alias tag="ctags -e -R ."

#
# Init script version managers
#

if [ -e /opt/homebrew/bin/brew ]; then
    augment_path /opt/homebrew/bin
fi

if $(command_exists brew); then
    HOMEBREW_PREFIX="$(brew --prefix)"

    # Use OpenJDK if homebrew has installed it.
    if [ -d "${HOMEBREW_PREFIX}/opt/openjdk" ]; then
        export JAVA_HOME="${HOMEBREW_PREFIX}/opt/openjdk"
    fi

    eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"

    NVM="$HOMEBREW_PREFIX/opt/nvm"
    if [ -e "$NVM/nvm.sh" ]; then
        export NVM_DIR="$HOME/.nvm"
        [ -s "$NVM/nvm.sh" ] && . "$NVM/nvm.sh"  # This loads nvm
        [ -s "$NVM/etc/bash_completion.d/nvm" ] && . "$NVM/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
    fi

    # asdf looks nice, but so far it has created problems running Electron in VSCode.
    #
    # if [ -e "$HOMEBREW_PREFIX/opt/asdf/libexec/asdf.sh" ]; then
    #     . "$HOMEBREW_PREFIX/opt/asdf/libexec/asdf.sh"
    # fi
fi

if $(command_exists rbenv); then
    eval "$(rbenv init -)"
fi
if $(command_exists pyenv); then
    augment_path "${HOME}/.pyenv/shims"
    eval "$(pyenv init -)"
fi

# Go

export GOPATH=~/code/go
alias goc='go build'
alias got='go test $(go list ./... | grep -v /vendor/)'
alias gotag="find . -type f -name '*.go' | xargs ctags -e"

# Docker

alias dks='docker stop $(docker ps -q)'
alias dkl='docker logs -f $(docker ps -q)'

dockerb() {
    docker exec -it $1 /bin/bash
}

# Python

export VIRTUAL_ENV_DISABLE_PROMPT=1
alias venv-activate='source .venv/bin/activate'
alias venv-show='if [ -n "${VIRTUAL_ENV+set}" ];
    then echo $VIRTUAL_ENV
fi'
alias ispark='PYSPARK_DRIVER_PYTHON=ipython pyspark'

augment_path ~/bin:~/.binac

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
    export BASH_SILENCE_DEPRECATION_WARNING=1

    if [ -z "${JAVA_HOME+set}" ]; then
        export JAVA_HOME=/Library/Java/Home
    fi

    # Aliases for opening files in various applications

    alias mn=man_preview
    alias em="open -a Emacs"
    alias emc='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    alias mvi='open -a MacVim'

    alias code='"/Applications/Visual Studio Code.app//Contents/Resources/app/bin/code"'

    alias brave="open -a Brave\ Browser"
    alias chromium="open -a Chromium"

    alias fork="open -a Fork"

    INTELLIJ=`ls -1d /Applications/IntelliJ\ * 2>/dev/null | tail -n1`
    if [ -d "$INTELLIJ" ]; then
        alias intellij="open -a \"$INTELLIJ\""
    fi

    DO_TOKEN=`security find-generic-password -a digital-ocean-token -w 2> /dev/null`
    if ! [ -z $DO_TOKEN ] && [ -e ~/.ssh/id_ed25519.pub ]; then
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

export PATH
