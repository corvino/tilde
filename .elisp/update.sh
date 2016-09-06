#! /bin/bash

curl 'https://www.emacswiki.org/emacs/download/apache-mode.el' > ~/.elisp/apache-mode.el
curl 'https://www.emacswiki.org/emacs/download/diff-mode-.el' > ~/.elisp/diff-mode-.el
curl 'http://jblevins.org/projects/markdown-mode/markdown-mode.el' > ~/.elisp/markdown-mode.el
curl 'https://www.emacswiki.org/emacs/download/etags-select.el' > ~/.elisp/etags-select.el
curl 'https://raw.githubusercontent.com/google/protobuf/master/editors/protobuf-mode.el' > ~/.elisp/protobuf-mode.el

# SML Mode (http://elpa.gnu.org/packages/sml-mode.html)
#
# This string of Perl gunk comes from http://blog.binchen.org/posts/no-worries-when-elpa-is-down.html
# It means that the latest version will be parsed from the ELPA directory. So kind of a win?
curl -L https://elpa.gnu.org/packages/archive-contents | perl -pe 's/(^\(1|\n)//g' | perl -pe 's/\]\)/])\n/g' | perl -pe 's/^ *\(([a-z0-9A-Z-]*).*\[\(([0-9 ]*).*(single|tar).*/\1-\2.\3/g' | perl -pe 's/ /./g' | perl -pe 's/single/el/g' | perl -pe 's/\)//g' | grep -i -e 'sml-mode' | xargs -I {} curl -L  https://elpa.gnu.org/packages/{} -o ~/.elisp/sml-mode.el
