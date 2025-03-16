(defun printd (object)
  "Print to and display *debug* buffer."
  (print object (get-buffer-create "*debug*"))
  (display-buffer "*debug*"))

(defun lappend (element list)
  "Return a new list with the specified element appended to list"
  (reverse (cons element (reverse list))))

(defun take-column (col list)
  "Take the col item of each list in list."
  (mapcar (apply-partially 'nth col) list))

(defun title-collapse ()
  (interactive)
  (save-excursion
    (search-backward "\"" nil nil)
    (set-mark-command)
    (search-forward "\"" nil nil)
    (setq title  (buffer-substring (region-beginning) (region-end)))
    (printd title)
    )
)

;; Here is a few functions that build a "collapse and kill" interactive
;; function. The collapse is a transform that capitalizes all word
;; beginnings and removes spaces, the target is the text at point
;; surrounded by parantheses.

;; Alternative implementation that doesn't set mark, but is kind-of hard
;; to read.
;;
;; See here for inspiration:
;;   https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
(defun string-from-quotes ()
  (interactive)
  (save-excursion
    (search-backward "\"")
    (let ((start (+ (point) 1)))
      (search-forward "\"" nil nil 2)
      (let ((end (- (point) 1)))
        (buffer-substring-no-properties start end)))))

(defun string-from-quotes ()
  (save-mark-and-excursion
    (search-backward "\"")
    (forward-char 1)
    (set-mark-command nil)
    (search-forward "\"")
    (backward-char 1)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun title-collapse ()
  (interactive)
  (kill-new (replace-regexp-in-string " " "" (capitalize (string-from-quotes))))
)

(global-set-key [f6] 'title-collapse)


;; Functions for getting info from org-mode links.
;; Definitely needs some cleanup (there is duplication, for one thing).

(defun org-link-extract-link ()
  "Extract link from org-mode link and add it to kill ring."
  (interactive "P")
  (let ((element (org-element-lineage (org-element-context) '(link) t)))
    (org-element-property :raw-link element)))

(defun org-link-extract-title ()
  "Extract link from org-mode link and add it to kill ring."
  (let* ((element (org-element-lineage (org-element-context) '(link) t))
         (start (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element)))
    (buffer-substring-no-properties start end)))

;; Kill components from link

(defun org-link-copy-title ()
  (interactive)
  (let ((title (org-link-extract-title)))
    (kill-new title)))

(defun org-link-copy-link ()
  (interactive)
  (let ((link (org-link-extract-link)))
    (kill-new link)))

(global-set-key [f7] 'org-link-copy-title)
(global-set-key [f8] 'org-link-copy-link)

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
