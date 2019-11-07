The most active of these is certainly .emacs, followed by .bashrc. These
days I spend most of my time on OS X&mdash;where I use the latest GNU
Emacs release. At least for the moment I've given up most pretences of
maintaining OS/Emacs cross-compatibility. Although I expect most of the
basics will work anywhere.

Setup
-----

There is a setup.sh bash script that creates symlinks from the ~ to the
appropriate files.

Before creating the symlinks, the script also attempts to move any
conflicting files from ~ to BACKUP. If a file is already appropriately
symlinked, it is skipped. BACKUP is only created if it is needed; and if
it is needed and exists, the script does not attempt to backup or link.

The setup process is simple: clone and run setup.sh.

Remote Tasks
------------
Ansible is nice for grooming remote hosts.

The first playbook to run is one to push authorized keys to the remote host, in this case installKeys.yml. This requires [sshpass](https://gist.github.com/arunoda/7790979), which can be installed with:

    brew install https://raw.githubusercontent.com/kadwanev/bigboybrew/master/Library/Formula/sshpass.rb
    # or
    apt-get install sshpass

Then run:

    # The comma causes Ansible to interpret the inventory as hostname/ip adress.
    # -k (--ask-pass) causes ansible to ask for connection password
    ansible-playbook -i <address>, installKeys.yml -k

Emacs
-----

.emacs attempts to be as lightweight as possible. It avoids any package
manager&mdash;Emacs seems to have at least 37 of them, so just deciding
on one seems prohibitively complicated.  The bash script
~/.elisp/update.sh uses curl to download any needed dependencies to
~/.elsip.

These dependencies are checked in, so updating is not necessary to get
started, and git can be used following the update to see what, if
anything, changed.
