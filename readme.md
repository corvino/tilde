The most active of these is certainly .emacs, followed by .bashrc. These days I
spend most of my time on OS X&mdash;where I use the latest GNU Emacs
release. At least for the moment I've given up most
pretences of maintaining OS/Emacs cross-compatibility. Although I expect
most of the basics will work anywhere.

Setup
-----

There is a setup.sh bash script that creates symlinks from ~ to
~/.dotfiles/. I suppose there is no reason why it couldn't create
symlinks from where the bash script is run, but currently it
doesn't. Maybe tomorrow.

The script also attempts to move any conflicting files from ~ to
~/dotfiles_old before creating the symlinks. It is careful not to delete
anything; if ~/dotfiles_old already exists, it does nothing.

The setup process is simple: clone to ~/.dotfiles and run
~/.dotfiles/setup.sh.

Emacs
-----

.emacs attempts to be as lightweight as possible. It avoids any package
manager&mdash;Emacs seems to have at least 37 of them, so just deciding
on once seems prohibitively complicated.  Instead the bash script
~/.elisp/update.sh uses curl to download any needed dependencies to
~/.elsip.

These dependencies are checkd in, so updating is not necessary to get
started so git can be used following the update to see if anything
changed.
