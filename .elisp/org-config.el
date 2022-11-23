(require 'ol)

;; Custom org-mode link schemese

(defun org-app-open (app path)
  (open-in-app app (expand-file-name path))
)

(defun org-iterm-open (path _)
  (org-app-open "iTerm.app" path)
)

(defun org-vscode-open (path _)
  (org-app-open "Visual Studio Code.app" path)
)

(defun org-xcode-open (path _)
  (org-app-open (car (last (directory-files "/Applications" nil "Xcode.*\\.app"))) path)
)

(defun org-vscode-open (path _)
  (org-app-open "IntelliJ IDEA.app" path)
)

(org-link-set-parameters "iterm" :follow #'org-iterm-open)
(org-link-set-parameters "vscode" :follow #'org-vscode-open)
(org-link-set-parameters "xcode" :follow #'org-xcode-open)
(org-link-set-parameters "intellij" :follow #'org-intellij-open)
