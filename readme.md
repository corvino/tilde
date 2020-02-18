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

Git Scripts
-----------
* **git-sub-stats.sh**
  Uses find to traverse the tree for the CWD and find any `.git` directories, indicating git repos (possibly submodules). Outputs stats on those repos.
  * _-o, --only-branches_
    Only show repos that have branches checked out (no headless tag or commit checkouts).
  * _-n, --no-current_
    Does not show the CWD repo.
  * _-c, --no-commit_
    Adds a column with the commit hash for each repo.
  * _-p, --preserve-separator_
    Instead of pretty-formatting with `column`, leaves the fields separated by the more machine-friendly `:`. Useful for passing the output to other scripts, such as `git-sub-sync.sh`.
* **git-sub-sync.sh**
  Fetches and merges the git repos with the stdin input format of <subdirectory>:<branch>. Each line represents a repo to update to the latest version of the specified branch. Calls `git fetch`, `git checkout <branch>`, and `git merge` on each repository. Lines beginning with `#` are treated as comments and ignored.

  This is compatible with output format of `git-sub-stats.sh -o -n -p`, which outputs three fields for each repo--the path to the subdirectory, the branch the repo is on, and the date of the last update. `git-sub-sync.sh` simply ignored additional fields after the first two.

  Example:

      git-sub-sync.sh < submodule-targets.data


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

To push .tilde and run setup to a remote machine:

    ansible-playbook -v -i <address>, installTilde.yml

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
