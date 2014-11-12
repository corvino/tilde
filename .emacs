;; -*- Mode: Emacs-Lisp; comment-column: 62 -*-
;; ==========================================================================
;;
;; Copyright © 1997-2014  Nathan Corvino

(if (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode 0))
(if (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode nil))

;; Set Variables.

(blink-cursor-mode (- (*) (*) (*)))
(setq           load-path (append load-path '("~/.elisp/"))
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
                fill-column 72
                show-trailing-whitespace t)
(if (eql system-type 'darwin)
    (progn
      (setq ns-command-modifier 'meta)
      (setq ns-pop-up-frames nil)))

(if (fboundp 'paren-set-mode)
    (paren-set-mode 'sexp)
  (setq show-paren-style 'expression)
  (show-paren-mode))


;; Bind most frequently used commands to single keys when possible.

(global-set-key "\M-o" 'other-window)

(global-set-key [f1] 'delete-other-windows)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'split-window-right)
(global-set-key [f10] 'delete-window)

(global-set-key [(control \;)] 'etags-select-find-tag-at-point)
(global-set-key [(control \')] 'etags-select-find-tag)

;; Use \C-z as namespace for custom keybindings.

(global-set-key "\C-z" nil)
(global-set-key "\C-zl" 'goto-line)
(global-set-key "\C-zb" 'build-tags)
(global-set-key "\C-zv" 'visit-tags)
(global-set-key [?\C-z backspace] 'revert-buffer)
(global-set-key "\C-z/" 'comment-region)
(global-set-key "\C-z?" 'uncomment-region)
(global-set-key "\C-z[" 'decrease-left-margin)
(global-set-key "\C-z]" 'increase-left-margin)
(global-set-key "\C-zk" 'compile)
(global-set-key "\C-zd" 'gdb)
(global-set-key "\C-zw" 'gdb-many-windows)
(global-set-key "\C-ze" 'gdb-restore-windows)

;; Key bindings for custom functions.

(define-key global-map [(control ?z) ?t] 'de-pollinate)
(define-key global-map [(control ?z) ?p] 'goto-matching-paren)
(define-key global-map [(control ?z) ?o] 'browse-selected-file)

(fset 'yes-or-no-p 'y-or-n-p)
(if (fboundp 'xterm-mouse-mode) (xterm-mouse-mode t))

(load-library "etags-select.el")

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

;; SML

(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(defun sml-cmd-override ()
  "Override sml--read-run-cmd to specify the sml command."
  '("/usr/local/bin/sml" "" ""))

(add-hook 'sml-mode-hook
          (lambda()
            (defalias 'sml--read-run-cmd 'sml-cmd-override)))

;; Allow sml-run/run-sml before loading sml-mode.
(defalias 'run-sml 'sml-run)
(defun sml-run ()
  "Load sml-mode and run sml-run with sml-cmd-override to avoid
prompting for the sml command. sml-mode overrides this on load."
  (interactive)
  (load "sml-mode")
  (apply 'sml-run (sml-cmd-override)))

;; Customize modes

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4)))

(add-hook 'html-mode-hook
  '(lambda ()
     ;;(setq indent-tabs-mode nil)
     (define-key html-mode-map "\C-j" 'insert-newline-and-indent-relative)
     (define-key html-mode-map "\t" 'indent-next-stop)))

(add-hook 'js-mode-hook
   '(lambda ()
      (setq js-indent-level 2)))

;; Custom functions

(defun de-pollinate ()
  "Trailing whitespace is the polline of code."
  (interactive)
  (delete-trailing-whitespace nil nil))

(defun goto-matching-paren ()
  "If point is sitting on a parenthetic character, jump to its match."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((progn
           (backward-char 1)
           (looking-at "\\s\)")) (forward-char 1) (backward-list 1))))

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

(defun trim (str)
  "Trim leading and tailing whitespace from str."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun locate-buffer-dominator (file)
  "Locates the first directory above the buffer file's directory
that contains an file named as specified."
  (if buffer-file-name
      (let ((dir (file-name-directory buffer-file-name))
            (root (locate-dominating-file (file-name-directory buffer-file-name) file)))
        (if root
            root
          (error (concat "Could not locate " file " file dominating " dir))))
    (error (concat "Could not locate " file " file; buffer has no file name."))))

(defun build-tags ()
  "Build etags using ctags in the director containg .git located
above of current buffer's directory."
  (interactive)
  (let ((root (locate-buffer-dominator ".git")))
    ;; FIXME: This shouldn't be a hard coded path, but currently
    ;; the ctags being invoked is /usr/bin/ctags, which appears to
    ;; invoke ctags within Xcode. Also need a ctags that supports
    ;; Objective-C (and Swift).
    (shell-command (concat "/usr/local/bin/ctags -e -R --extra=+fq --exclude=.git -f " root "/TAGS " root))
    (visit-tags)
    (message (concat "tags built for " root))))

(defun visit-tags ()
  "Visit the etags file TAGS located in direcotry above the
current buffer's directory."
  (interactive)
  (let ((root (locate-buffer-dominator "TAGS")))
    (visit-tags-table (concat root "/TAGS"))
    (message (concat "visited tags file in " root))))

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
