;; -*- Mode: Emacs-Lisp; comment-column: 62 -*-
;; ==========================================================================
;;
;; Nathan Corvino's .emacs File For XEmacs & GNU Emacs
;; Copyright © 1997-2009  Nathan Corvino
;;
;;     This file is not a part of XEmacs or GNU Emacs
;;
;;     This program is free software; you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation; either version 2, or (at your option)
;;     any later version.
;;
;;     This program is distributed in the hope that it will be useful, but
;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;     General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with this program; see the file COPYING.  If not, write to the
;;     Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Acknowledgements:
;;
;;     Andrew Kensler's has provided much Emacs advice and tutelage, as
;;     well as some excellent ELisp in his init.el.
;;
;; Change Log:
;;

;; Move the frame to a resonable starting position and size.  Are screens aren't from 1988.

;; Turn off wasteful inteface elements.

(if (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode 0))
(if (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode nil))

;; Emacs starts small by default.  Position and resize.

(set-frame-position (selected-frame) 0 0)
(set-frame-size (selected-frame) 169 71)

;; Set Variables.

(blink-cursor-mode (- (*) (*) (*)))
(setq           load-path (append load-path '("~/.elisp/" "~/.elisp/ocaml" "~/.elisp/emacs-rails" "~/.elisp/php-mode-1.5.0"))
                backup-directory-alist (quote ((".*" . "~/.emacs.d/autosave")))
                tramp-backup-directory-alist (quote ((".*" . "~/.emacs.d/autosave")))
                make-backup-files t
                global-font-lock-mode t
                track-eol t
                truncate-partial-width-windows nil
                column-number-mode t
                line-number-mode t
                visible-bell t
                initial-scratch-message nil
                inhibit-startup-message nil
                ;;standard-indent 4
                ;;standard-indent 2
                standard-indent 4
                ;;tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92))
                tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92))
                ;;tab-stop-list '(8 16 32 40 48 56 64 72 80 88))
(setq-default   tab-width 4
                indent-tabs-mode nil
                fill-column 72)
(if (eql system-type 'darwin)
    (progn
      (setq ns-command-modifier 'meta)
      (setq ns-pop-up-frames nil)))

(if (fboundp 'paren-set-mode)
    (paren-set-mode 'sexp)
  (setq show-paren-style 'expression)
  (show-paren-mode))

;; Use \C-z ... for custom key bindings.  That way default emacs
;; bindings are still in place.

(global-set-key "\C-z" nil)
(global-set-key "\C-zl" 'goto-line)
(global-set-key [?\C-z backspace] 'revert-buffer)
(global-set-key "\C-z/" 'comment-region)
(global-set-key "\C-z?" 'uncomment-region)
(global-set-key [?\C-z tab] 'indent-region)
(global-set-key "\C-z[" 'decrease-left-margin)
(global-set-key "\C-z]" 'increase-left-margin)
(global-set-key "\C-zk" 'compile)
(global-set-key "\C-zd" 'gdb)
(global-set-key "\C-zw" 'gdb-many-windows)
(global-set-key "\C-ze" 'gdb-restore-windows)

;; Key bindings for custom functions.

(define-key global-map [(control ?z) ?t] 'strip-trailing-space)
(define-key global-map [(control ?z) ?p] 'goto-matching-paren)
(define-key global-map [(control ?z) ?@] 'kill-other-buffers)
(define-key global-map [(control ?z) ?#] 'kill-all-buffers)
(define-key global-map [(control ?z) ?o] 'browse-selected-file)
(define-key global-map [(control ?z) ?m] 'toggle-mini-xemacs)

(fset 'yes-or-no-p 'y-or-n-p)
(if (fboundp 'xterm-mouse-mode) (xterm-mouse-mode t))

;; (load (expand-file-name "slimeinit" (expand-file-name ".elisp" (expand-file-name "~"))))

;;(require 'rdebug)
;;(require 'rails)

;; Autoload additional modes.

;; Pretty diff mode
(autoload 'diff-mode- "diff-mode-" "Fancy diff display" t)
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
  "Intelligent Emacs interface to diff")

(autoload 'apache-mode "apache-mode" "Major mode for editing Apache configuration files" t)

;; Map files to modes.

(add-to-list 'auto-mode-alist '("\\.conf$" . apache-mode) t)
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))

;; OCaml Modes

(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

(if window-system (require 'caml-font))

;; PHP
;; http://php-mode.sourceforge.net/

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Markdown
;;http://jblevins.org/projects/markdown-mode/

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; Customize modes

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4)))

;; (add-hook 'c++-mode-hook
;;   '(lamdbda ()
;;     (c-set-style "Stroustrup")
;;     (c-toggle-auto-state)))

;; For working with Weblingo files.
;; We should figure out how to do this on-demand for any buffer.

(add-hook 'html-mode-hook
  '(lambda ()
     ;;(setq indent-tabs-mode nil)
     (define-key html-mode-map "\C-j" 'insert-newline-and-indent-relative)
     (define-key html-mode-map "\t" 'indent-next-stop)))

;; Custom functions

(defun strip-trailing-space ()
  "Strip out trailing whitespace from all lines in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" t t))))

(defun goto-matching-paren ()
  "If point is sitting on a parenthetic character, jump to its match."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((progn
           (backward-char 1)
           (looking-at "\\s\)")) (forward-char 1) (backward-list 1))))

(defun kill-other-buffers ()
  "Kill all buffers except the current and unsplit the window."
  ;; As akensler says, Emacs buffers multiply faster than rabbits.  It
  ;; should be easy to kill them.
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))   ; Delete other buffers
  (delete-other-windows)                                      ; And then unsplit the current window...
  (delete-other-frames))                                      ; ...and remove other frames, too.

(defun kill-all-buffers()
  "Kill all buffers, including current, and unsplit the window."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows)
  (delete-other-frames))

(defun browse-selected-file ()
  "Opens a file in a browser .  If a region is selected, the text of the
highlighted region will be used as the filename or URL to load.  If no
region is active try to browse to the file being visited."
  (interactive)
  (if (if (fboundp 'region-exists-p)
          (region-exists-p)
        (and transient-mark-mode mark-active))
      (browse-url (buffer-substring (save-excursion (goto-char (region-beginning))
                                                    (skip-chars-forward " \t\n\r")
                                                    (point))
                                    (save-excursion (goto-char (region-end))
                                                    (skip-chars-backward " \t\n\r")
                                                    (point))))
    (browse-url (buffer-file-name))))

(defun toggle-mini-emacs ()
  "Switches back and forth between minimal look for Emacs."
  (interactive)
  (if (fboundp 'specifier-instance)
      ;; For XEmacs.
      (let ((setting (not (specifier-instance menubar-visible-p))))
        (set-specifier menubar-visible-p setting)
        (set-specifier top-toolbar-visible-p setting)
        (set-specifier top-gutter-visible-p setting))
    (if (fboundp 'tool-bar-mode)
        ;; For GNU Emacs.
        (let ((setting (if tool-bar-mode 0 1))
              (scroll-setting (if tool-bar-mode nil 'right)))
          (scroll-bar-mode scroll-setting)
          (tool-bar-mode setting)
          (menu-bar-mode setting)))))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun insert-newline-and-indent-relative ()
  "Inserts a newline and indents it to the width of the last line."
  (interactive)
  (newline)
  (indent-relative))

(defun indent-next-stop ()
  "Indents to the next tab stop, using tab or spaces accodring to indent-tab-mode."
  "An interactive wrapper around insert-tab from indent.el."
  (interactive)
  (insert-tab))

(defun rigid-body-c++-mode ()
  "Stupid 2-space tabs."
  (interactive)
  (c-mode)
  (setq c-basic-offset 2
        c-indent-level 2
        standard-indent 2))

(setq auto-mode-alist (cons '(".*/RigidBodies/.*\\.\\(cpp\\|h\\)$" . rigid-body-c++-mode) auto-mode-alist))

(defun linux-c-mode ()
  "All the stuff needed to setup for the linux coding standard."
  "Whatever you do, don't piss off Linus!"
  (interactive)
  (c-mode)
  (setq c-basic-offset 8
        c-indent-level 8
        c-brace-imaginary-offset 0
        c-brace-offset -8
        c-arg-decl-indent 8
        c-label-offset -8
        c-continued-statement-offset 8
        indent-tabs-mode t
        tab-width 8))

(setq auto-mode-alist (cons '(".*/linux-msm-2.6.32/.*\\.[ch]$" . linux-c-mode) auto-mode-alist))
    ;; (add-hook 'c-mode-common-hook
    ;;           (lambda ()
    ;;             ;; Add kernel style
    ;;             (c-add-style
    ;;              "linux-tabs-only"
    ;;              '("linux" (c-offsets-alist
    ;;                         (arglist-cont-nonempty
    ;;                          c-lineup-gcc-asm-reg
    ;;                          c-lineup-arglist-tabs-only))))))
    ;; (add-hook 'c-mode-hook
    ;;           (lambda ()
    ;;             (let ((filename (buffer-file-name)))
    ;;               ;; Enable kernel mode for the appropriate files
    ;;               (when (and filename
    ;;                          (string-match (expand-file-name "~/src/linux-trees")
    ;;                                        filename))
    ;;                 (c-set-style "linux-tabs-only"))))))

;; (defun linux-c-mode-local
;;   "Set c mode for Linux kernel on buffer."
;;   (interactive)
;;   (make-local-variable 'c-basic-offset)
;;   (make-local-variable 'indent-tabs-mode)
;;   (linux-c-mode))


;; (defun linux-c-mode ()
;;   "C mode with adjusted defaults for use with the Linux kernel."
;;   (interactive)
;;   (c-mode)
;;   (c-set-style "K&R")
;;   (setq c-basic-offset 8)
;;   (setq-default indent-tabs-mode t))
