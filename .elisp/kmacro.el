(defun set-last-kmacro ()
  "Sets last-kbd-macro using the string surrounding point."
  (interactive)
  (let* ((kmacro-str (string-at-point))
         (kmacro (kmacro kmacro-str)))
    (if (not (kmacro-p kmacro))
        (error "Unable to load macro."))
    (message "Set last-kbd-macro to " kmacro-str)
    (setq last-kbd-macro (kmacro--keys kmacro))))

(defun set-last-kmacro-by-name (kmacro-sym)
  "Sets last-kbd-macro using the kmacro defined in the specified symbol."
  (interactive
   ;; filter only kmacros; kmacro-sym is the symbol for a kmacro.
   ;;
   ;; (symbol-function 'name) will be the same as (kmacro string) where
   ;; there is (defalias 'name (kmacro string)).
   (list (intern
          (completing-read
           "kmacro name: "
           (let ((result))
             (mapatoms (lambda (it)
                         (when (kmacro-p (symbol-function it))
                           (push it result))))
             result)))))
  (setq last-kbd-macro (kmacro--keys (symbol-function kmacro-sym))))
