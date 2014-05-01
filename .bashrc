[ -z "$PS1" ] && return   # If not running interactively, don't do anything

#. ~/.bash/dircolors.bash
#. ~/.bash/pushhome.bash

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

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

#echo "bash $BASH_VERSINFO"
#export EMACSVERSION=`emacs --version | grep -E '[0-9]+' -o | head -1`


#########################
## OS w4118 randomness ##
#########################

HOMEWORK_NUMBER=4
ANDROID_SDK=~/android-sdk-os.f2010
LINUX_SOURCE=~/hmwk$HOMEWORK_NUMBER/linux-msm-2.6.32

ANDROID_DEVICE=Homework$HOMEWORK_NUMBER # Name of the android device to run in emulator.
EXEC_PATH=usr/prinfo # Path to executable within linux source tree.
EXEC_NAME=my_prinfo

alias armmake='make -j2 ARCH=arm CROSS_COMPILE=arm-none-linux-gnueabi-'
alias armmaked='make -j2 ARCH=arm CROSS_COMPILE=arm-none-linux-gnueabi- goldfish_defconfig'
alias aemu='emulator -avd $ANDROID_DEVICE -kernel $LINUX_SOURCE/arch/arm/boot/zImage -ramdisk $ANDROID_SDK/platforms/android-os.f2010/images/ramdisk.img -show-kernel'
alias aemud='emulator -avd $ANDROID_DEVICE -kernel $LINUX_SOURCE/arch/arm/boot/zImage -ramdisk $ANDROID_SDK/platforms/android-os.f2010/images/ramdisk.img -show-kernel -verbose -qemu -monitor telnet::4444,server -s&'
alias push2='adb push ~$LINUX_SOURCE/$EXEC_PATH/$EXEC_NAME_p /data/misc'
alias run2='adb shell /data/misc/$EXEC_NAME'
alias checklinux='git diff start | ./linux-msm-2.6.32/scripts/checkpatch.pl --no-signoff -'

#########################

#
# *** Aliases ***
#
# Now that we have started using aliases, it is hard to stop.
# Such a simple idea, yet so powerful.
#

alias gitego='git log | grep athan | wc -l'

alias syns='synergys --config ~/.synergy2'
alias synsdebug='synergys -f --debug DEBUG1 --config ~/.synergy2'
alias killsyn="kill `ps -ef | grep synergy | grep -v grep | awk '{print $2}'`"


#
# System-specific stuff.
#

if [ "darwin8.0" == $OSTYPE ] || # Tiger
    [ "darwin9.0" == $OSTYPE ] || # Leopard
    [ "darwin10.0" == $OSTYPE ] || # Snow Leopard
    [ "darwin11" == $OSTYPE ] # Lion
then

    # Macports paths

	export PATH=/usr/local/texlive/2011/bin/x86_64-darwin:$PATH
    #export PATH=/opt/local/lib/postgresql84/bin:$PATH
    #export PATH=/opt/local/bin:/opt/local/sbin:$PATH
    #export MANPATH=/opt/local/share/man:$MANPATH

    # MySQL path

    #export PATH=/usr/local/mysql/bin:$PATH

    export PATH=/usr/local/bin:$PATH

    export CLICOLOR=1
    export MACOSX_DEPLOYMENT_TARGET=10.6
    export JAVA_HOME=/Library/Java/Home

    alias emacs='open -a Emacs'
    alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
    #. ~/.bash/emacs.bash

elif [ "linux-gnu" == $OSTYPE ] ; then

    #. ~/.bash/ssh.bash
    eval `dircolors -b`

    if [ "$TERM" != "dumb" ]; then
        alias ls='ls --color=auto'
    fi

    # We source emacs.bash before we alias emacs, so that calls to emacs in
    # emacs.bash don't have the alias applied.

    #. ~/.bash/emacs.bash
    #alias emacs=editnowait
    
fi

# Allow one-offs to override as needed.

if [ -f ~/.bash_platform ] ; then
    . ~/.bash_platform
fi

if [[ -s "$HOME/.rvm/scripts/rvm" ]]  ; then source "$HOME/.rvm/scripts/rvm" ; fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
