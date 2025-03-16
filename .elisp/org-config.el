(require 'ol)

(setq org-startup-folded nil
      org-startup-truncated nil
      org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\M-[" 'org-metaleft)
            (local-set-key "\M-]" 'org-metaright)
            (local-set-key "\M-p" 'org-metaup)
            (local-set-key "\M-n" 'org-metadown)))

;; Custom org-mode link schemese

(defun org-app-open (app path)
  (open-in-app app (expand-file-name path)))

(defun org-iterm-open (path _)
  (org-app-open "iTerm.app" path))

(defun org-vscode-open (path _)
  (org-app-open "Visual Studio Code.app" path))

(defun org-xcode-open (path _)
  (org-app-open (car (last (directory-files "/Applications" nil "Xcode.*\\.app"))) path))

(defun org-intellij-open (path _)
  (org-app-open "IntelliJ IDEA.app" path))

(org-link-set-parameters "iterm" :follow #'org-iterm-open)
(org-link-set-parameters "vscode" :follow #'org-vscode-open)
(org-link-set-parameters "xcode" :follow #'org-xcode-open)
(org-link-set-parameters "intellij" :follow #'org-intellij-open)
