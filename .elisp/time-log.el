(defun tl-parse-clock-face (str)
  "Parse string of form HH:MM to timestamp."
  (time-convert (mapcar (lambda (x) (if (null x) 0 x)) (parse-time-string str))))

(defun tl-parse-pair (str)
  "Parse string of format h:m into (hour min)."
  (let* ((parts (parse-time-string str))
         (hour (caddr parts))
         (min (cadr parts)))
    (list hour min)))

(defun tl-pair-to-sec (x)
  "Take duration as (hour min) and calculate seconds"
  (* 60 (+ (* 60 (car x)) (cadr x))))

(defun tl-parse (str)
  "Parse string of format h:m to seconds."
  (tl-pair-to-sec (tl-parse-pair str)))

(defun tl-fmt-sec (sec)
  (format-seconds "%h:%.2m" sec))

(defun tl-interval (a b)
  "Calculate time interval from a to b where inputs are strings of format h:m."
  (let ((a (tl-parse-pair a))
        (b (tl-parse-pair b)))
    (if (> (car a) (car b)) (setcar b (+ 12 (car b))))
    (format-seconds "%h:%.2m" (- (tl-pair-to-sec b) (tl-pair-to-sec a)))))


(defun tl-total (times)
  "Total the time values in times."
  (tl-fmt-sec (seq-reduce #'+ (mapcar 'tl-parse times) 0)))

(defun time-log-summarize (log)
  "Take table in format (day start end duration project tasks)
and output summary of (day project total-time tasks)."
  (let ((days (seq-uniq (mapcar #'car log))))
    (mapcan
     (lambda (day)
       (let* ((items (seq-filter (lambda (item)  (equal day (car item))) log))
              (projects (seq-uniq (mapcar (apply-partially 'nth 4) items))))
         (mapcar (lambda (project)
                   (let* ((pitems (seq-filter (lambda (item) (equal project (nth 4 item))) items))
                          (time (tl-total (take-column 3 pitems)))
                          (comments (mapconcat #'identity (take-column 5 pitems) "; ")))
                     (list day project time comments)))
                 projects)))
     days)))

(defun tl-summary (log)
    "Add a total column to the end of the summary."
    (let* ((summary (time-log-summarize log))
           (times (mapcar (apply-partially 'nth 3) log))
           (total (list "Total" "" (tl-total times) "")))
      (lappend total summary)))
