(setq log '(
 ("<2022-07-05 Tue>" "8:30" "8:59" "0:29" "Project A" "Develop feature")
 ("<2022-07-05 Tue>" "8:30" "9:00" "0:30" "Project A" "Test feature")
 ("<2022-07-06 Wed>" "8:00" "8:30" "0:30" "Project A" "Retrospective")
 ("<2022-07-06 Wed>" "8:00" "8:30" "0:30" "Project A" "Planning")
 ("<2022-07-06 Wed>" "8:30" "8:59" "0:29" "Project B" "Standup")
 ("<2022-07-07 Thur>" "9:00" "5:15" "8:15" "Project C" "Some work on forgotten project")
))

log
(printd log)
(printd (tl-summary log))


(car log)

;;     (cons '(Total nil 13:44 nil) days))))



(defun tl-summary (log)
    "Add a total column to the end of the summary."
    (let* ((summary (time-log-summarize log))
           (times (mapcar (apply-partially 'nth 3) log))
           (total (list "Total" nil (tl-total times) nil)))
      (lappend total summary)))

;;    (cons (time-log-summarize log) '(Total nil 13:44 nil)))
;;    (push '(Total nil 13:44 nil) (time-log-summarize log)))
    (lappend '(Total nil 13:44 nil) (time-log-summarize log)))

'(Total nil 13:44 nil)
(printd (tl-summary log))


(cons 'd '(a b c))
(reverse (cons 'd (reverse '(a b c))))
(lappend 'd '(a b c))


