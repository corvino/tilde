;; -*- Mode: Emacs-Lisp; comment-column: 62 -*-
;; ==========================================================================
;;
;; Copyright © 1997-2014  Nathan Corvino
;;
;;     This file is not a part of XEmacs or GNU Emacs
;;

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
                standard-indent 4
                tab-stop-list (number-sequence 4 120 4))
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

(global-set-key "\M-o" 'other-window)

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

;; Pretty diff mode

(require 'diff-mode-)
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff" "Intelligent Emacs interface to diff")

;; Apache

(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; Markdown

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook (lambda () (when buffer-file-name (add-hook 'after-save-hook 'check-parens nil t))))
(add-hook 'markdown-mode-hook (lambda () (modify-syntax-entry ?\" "\"" markdown-mode-syntax-table)))

;; Customize modes

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4)))

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

(defun linux-c-mode ()
  "All the stuff needed to setup for the linux coding standard."
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
