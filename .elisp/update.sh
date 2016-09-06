#! /bin/bash

curl 'https://www.emacswiki.org/emacs/download/apache-mode.el' > ~/.elisp/apache-mode.el
curl 'https://www.emacswiki.org/emacs/download/diff-mode-.el' > ~/.elisp/diff-mode-.el
curl 'http://jblevins.org/projects/markdown-mode/markdown-mode.el' > ~/.elisp/markdown-mode.el
curl 'https://www.emacswiki.org/emacs/download/etags-select.el' > ~/.elisp/etags-select.el
curl 'https://raw.githubusercontent.com/google/protobuf/master/editors/protobuf-mode.el' > ~/.elisp/protobuf-mode.el

# SML Mode (http://elpa.gnu.org/packages/sml-mode.html)
curl 'http://elpa.gnu.org/packages/sml-mode-6.5.el' > 'sml-mode.el'
