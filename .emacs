;; -*- Mode: Emacs-Lisp; comment-column: 62 -*-
;; ==========================================================================


;; Bootstrap straight.el
;; https://github.com/radian-software/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'htmlize)

(if (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode 0))
(if (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode nil))

(let ((new-paths (list (expand-file-name "~/.binac") )))
  (setenv "PATH" (concat (mapconcat 'identity new-paths ":") ":" (getenv "PATH")))
  (setq exec-path (append new-paths exec-path)))

(add-to-list 'load-path "~/.elisp")

(load-library "util.el")
(load-library "time-log.el")
(load-library "org-config.el")

;; Set Variables.

(blink-cursor-mode (- (*) (*) (*)))
(setq           backup-directory-alist (quote ((".*" . "~/.emacs.d/autosave")))
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
                tab-stop-list (number-sequence 4 120 4)
                org-startup-indented t
                org-startup-folded nil
                org-startup-truncated nil
                org-confirm-babel-evaluate nil)
(setq-default   tab-width 4
                indent-tabs-mode nil
                fill-column 72
                show-trailing-whitespace t)

;; Restore emacs desktop and history on restart
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;; https://www.emacswiki.org/emacs/SaveHist
(when (display-graphic-p)
  (desktop-save-mode 1)
  (setq desktop-dirname "~/.emacs.d/")
  (setq savehist-additional-variables '(kill-ring kmacro-ring))
  (setq savehist-file "~/.emacs.d/savehist"))

;; Restore proper \C-j/ret behavior by disabling `electric-indent-mode`
;; enabled by default in 24.4. See
;; http://git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS.24
;; https://emacsredux.com/blog/2013/03/29/automatic-electric-indentation/
(if (fboundp 'electric-indent-mode)
    (electric-indent-mode 0))

(put 'upcase-region 'disabled nil)

(if (eql system-type 'darwin)
    (progn
      (setq ns-command-modifier 'meta)
      (setq ns-pop-up-frames nil)
      (if (eq window-system nil)
          ;; macOS terminal is weird.
          (progn
            ;; The terminal has started being wonky when pasting from
            ;; the clipboard. Workaround by defining an emacs keybinding
            ;; that invokes the pbpaste command.
            (defun pbpaste ()
              "Paste for macOS clipboard."
              (interactive)
              (interactive)
              (insert (shell-command-to-string "pbpaste")))
            (global-set-key "\M-i" 'pbpaste)))))

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

;; Some macOS-like key bindings

(global-set-key "\M-[" 'decrease-left-margin)
(global-set-key "\M-]" 'increase-left-margin)
(global-set-key "\M-z" 'undo)
(global-set-key [?\C-/] 'comment-region)
(global-set-key [?\M-/] 'uncomment-region)

;; Editing

(global-set-key (kbd "C-S-s") 'query-replace)

;; Use \C-z as namespace for custom keybindings.

(global-set-key "\C-z" nil)
(global-set-key "\C-zl" 'goto-line)
(global-set-key [?\C-z backspace] 'revert-buffer)

;; etags

;; etags-select has been removed because tags weren't being used, but
;; would be worth investigating if a "return to tags" is made.

;; These got changed in emacs 25. Set them back to what they were
;; before. Maybe investigate at some point.
(global-set-key "\M-." 'find-tag)
(global-set-key "\M-," 'tags-loop-continue)
(global-set-key "\M-*" 'pop-tag-mark)

(global-set-key "\C-zt" 'visit-tags)
(global-set-key "\C-zb" 'build-tags)
(global-set-key (kbd "\C-z \C-f") 'tags-search)
(global-set-key (kbd "\C-z \C-a") 'tags-apropos)

;; Key bindings for custom functions.

(define-key global-map [(control ?z) ?s] 'de-pollinate)
(define-key global-map [(control ?z) ?p] 'goto-matching-paren)
(define-key global-map [(control ?z) ?o] 'browse-selected-file)
(global-set-key "\C-za" 'open-in-brave)

(fset 'yes-or-no-p 'y-or-n-p)
(if (fboundp 'xterm-mouse-mode) (xterm-mouse-mode t))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\M-[" 'org-metaleft)
            (local-set-key "\M-]" 'org-metaright)
            (local-set-key "\M-p" 'org-metaup)
            (local-set-key "\M-n" 'org-metadown)
            ))

;; Customize modes

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4)))

(add-hook 'html-mode-hook
  '(lambda ()
     ;;(setq indent-tabs-mode nil)
     (define-key html-mode-map "\t" 'indent-next-stop)
     (setq tab-stop-list (number-sequence 4 120 4))
     (setq tab-width 4)))

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
that contains a file named as specified."
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
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; auto-source any .el files in ~/.elips_auto_src
(if (file-directory-p "~/.elispac")
    (mapc
     (lambda (x)
       (load-file x))
     (directory-files "~/.elispac" t ".el$")))
(put 'downcase-region 'disabled nil)

(defun delete-buffer-file ()
  "Delete current buffer and file backing it."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "No file backing buffer")
      (if (y-or-n-p (format "Delete %s?" file))
          (progn
            (delete-file file)
            (kill-buffer)
            (message "Deleted file %s" file))))))
