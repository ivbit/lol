;;;; CHAPTER 14 (Land Of Lisp)

;; clear screen START
(defun clear ()
  #+clisp
    (ext:run-program "/usr/bin/clear")
  #+sbcl 
    (sb-ext:run-program "/usr/bin/clear" 'nil :input 'nil :output *standard-output*)
  #+clozure 
    (ccl:run-program "/usr/bin/clear" 'nil :input 'nil :output *standard-output*)
  '(screen was cleared)
) ; defun clear END
(defun clear0 () (clear))
(defun cls () (clear))
;; clear screen END

;;;; Functional Programming

;; The clear, functional part
(defun add-widget (database widget)
  (cons widget database)
) ; defun add-widget END

;; The dirty, nonfunctional part
(defparameter *database* nil)

(defun main-loop ()
  (loop
    (princ "Please enter the name of a new widget: ")
    (finish-output)
    (setf *database* (add-widget *database* (read)))
    (format t "The database contains the following: ~a~&" *database*)
  ) ; loop END
) ; defun main-loop END

(defparameter *my-list* '(4 7 2 3))

;; Code composition with imperative code
;; For demostration purposes only. A lisper would not write code like this.
(loop
  for n below (length *my-list*)
  do
    (setf (nth n *my-list*) (+ (nth n *my-list*) 2))
) ; loop END

;; Using the functional style
;; Without using higher-order programming:
(defun add-two (list)
  (when list
    (cons (+ 2 (car list)) (add-two (cdr list)))
  ) ; when END
) ; defun add-two END

; (add-two '(4 7 2 3))

;; Higher-order programming
(mapcar
  (lambda (x)
    (+ x 2)
  ) ; lambda END
  '(4 7 2 3)
) ; mapcar END





