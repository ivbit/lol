;; sbcl/ccl
(defparameter ☀ "6:00 AM")
(princ (coerce '(#\BLACK_SUN_WITH_RAYS #\ALARM_CLOCK #\FIRE #\PILE_OF_POO) 'string))
(terpri)
(let ((⏰ 0)) (defun ⏰ () (format t "~d:00~&" ⏰) (incf ⏰)))
(defun 💩 () (princ `(time-to-go ,☀)))
(loop for 🔥 upto 6 do (if (< 🔥 6) (⏰) (💩)))
