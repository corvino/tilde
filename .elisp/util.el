(defun printd (object)
  "Print to and display *debug* buffer."
  (print object (get-buffer-create "*debug*"))
  (display-buffer "*debug*"))

(defun take-column (col list)
  "Take the col item of each list in list."
  (mapcar (apply-partially 'nth col) list))

(defun url-from-org-link ()
  "Returns the url/destination of on org-mode link."
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 (buffer-substring-no-properties
                  (or (cadr link-info) (point-min))
                  (or (caddr link-info) (point-max))))))
    (if (not text)
        nil
      (string-match org-link-bracket-re text)
      (substring text (match-beginning 1) (match-end 1)))))

(defun open-in-app (application url)
  "Use open to open a url in a specified macOS application."
  (start-process "browse" nil "open" "-a" application url))

(defun open-in-brave ()
  "Open an org-mode link in brave."
  (interactive)
  (if-let ((url (url-from-org-link)))
      (open-in-app "Brave Browser.app" url)))
