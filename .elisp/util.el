(defun printd (object)
  "Print to and display *debug* buffer."
  (print object (get-buffer-create "*debug*"))
  (display-buffer "*debug*"))

(defun take-column (col list)
  "Take the col item of each list in list."
  (mapcar (apply-partially 'nth col) list))
