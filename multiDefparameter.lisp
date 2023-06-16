;;;; MULTIPLE VARIABLE ASSIGNMENT macros,
;;;; built on top of 'defparameter' and 'defvar'

;; clear screen (Unix-like systems) START
(defun clear ()
"Clears screen on Unix-like systems."
 #+clisp
  (ext:run-program "/usr/bin/clear")
 #+sbcl
  (sb-ext:run-program "/usr/bin/clear" nil :input nil :output *standard-output*)
 #+clozure
  (ccl:run-program "/usr/bin/clear" nil :input nil :output *standard-output*)
 '(screen was cleared)
) ; defun clear END
(compile 'clear)
(defun clear0 () (clear))
(compile 'clear0)
(defun cls () (clear))
(compile 'cls)
;; Modern terminal emulators only:
(defparameter clear (format nil "~c[3J~c[1;1H~c[0J" #\Esc #\Esc #\Esc))
(defparameter clear0 clear)
(defparameter cls clear)
;; clear screen (Unix-like systems) END

(defun param-pairs (lst)
"
Returns pairs (VAR VAL) from LIST, in which VAR cannot be NIL.
If LIST has an odd number of elements, value of last VAR is set to NIL.
Example:
(param-pairs '(foo 44 bar 87 baz 31))
(param-pairs '(foo (+ 2 3) bar nil baz (vector 1 2 3)))
(param-pairs '(foo 44 bar 87 baz 31 abc))
(param-pairs '(foo))
"
  (let
    ( ; variable definition START
      (prms
        (reduce
          (lambda (x y)
            (cond
              ( ; 1st condition START
                (null x)
                (list (list y))
              ) ; 1st condition END
              ( ; 2nd condition START
                (cdar x)
                (cons (list y) x)
              ) ; 2nd condition END
              ( ; default condition START
                t
                (setf (cdar x) (list y))
                x
              ) ; default condition END
            ) ; cond END
          ) ; lambda END
          lst
          :initial-value nil
        ) ; reduce END
      ) ; prms END
    ) ; variable definition END
    ; version where only pairs allowed, 'abc' is discarded:
    ; (param-pairs '(foo 44 bar 87 baz 31 abc))
    ; (if (cdar prms)
    ;   (reverse prms)
    ;   (reverse (cdr prms))
    ; ) ; if END
    (if (cdar prms)
      (reverse prms)
      (when (or (caar prms) (> (length prms) 1))
        (reverse (cons (list (caar prms) nil) (cdr prms)))
      ) ; when END
    ) ; if END
  ) ; let END
) ; defun param-pairs END
(compile 'param-pairs)
; (describe #'param-pairs)
; (documentation 'param-pairs 'function)
; (param-pairs '(foo 44 bar 87 baz 31))

;; Flawed version of 'mdefparameter'
(defmacro mdefparameter (&body body)
  `(let
    ( ; variable definition START
      (lst
        (mapcar
          (lambda (pair)
            (unless (null (car pair))
              (list 'defparameter (car pair) (cadr pair))
            ) ; unless END
          ) ; lambda END
          (param-pairs ',body)
        ) ; mapcar END
      ) ; lst END
    ) ; variable definition END
    (mapc #'eval lst)
  ) ; let END
) ; multi-defparameter END

;; Flawed version of 'mdefparameter'
(defmacro mdefparameter (&body body)
"
Defines multiple global variables at once. Variable name must not be NIL.
If there is only 1 variable name provided, that variable's value is set to NIL.
If 'mdefparameter' has odd number of arguments, the last variable's value is set to NIL.
If no arguments are provided, or single NIL argument is provided, returns NIL.
If variables with same names are already defined, changes their values.
Examples:
(mdefparameter *foo* (vector 1 2 3) *bar* nil *baz* 'hello *abc* (mapcar #'sqrt '(10 20 30)) *xyz*)
(mdefparameter *foo*)
(macroexpand '(mdefparameter foo 44 bar 77 baz))
"
  (let
    ( ; variable definition START
      (lst ; This variable exists only at 'macro expand time'
        (mapcar
          (lambda (pair)
            (unless (null (car pair))
              (list 'defparameter (car pair) (cadr pair))
            ) ; unless END
          ) ; lambda END
          (param-pairs body)
        ) ; mapcar END
      ) ; lst END
    ) ; variable definition END
    `(mapc #'eval ',lst) ; This list will be the body of function at 'runtime'
  ) ; let END
) ; defmacro mdefparameter END
(compile 'mdefparameter)
; (describe (macro-function 'mdefparameter))
; (documentation 'mdefparameter 'function)

;; Flawed version of 'mdefvar'
(defmacro mdefvar (&body body)
"
Defines multiple global variables at once. Variable name must not be NIL.
If there is only 1 variable name provided, that variable's value is set to NIL.
If 'mdefvar' has odd number of arguments, the last variable's value is set to NIL.
If no arguments are provided, or single NIL argument is provided, returns NIL.
If variables with same names are already defined, does not change their values.
Examples:
(mdefvar *foo* (vector 1 2 3) *bar* nil *baz* 'hello *abc* (mapcar #'sqrt '(10 20 30)) *xyz*)
(mdefvar *foo*)
(macroexpand '(mdefvar foo 44 bar 77 baz))
"
  (let
    ( ; variable definition START
      (lst ; This variable exists only at 'macro expand time'
        (mapcar
          (lambda (pair)
            (unless (null (car pair))
              (list 'defvar (car pair) (cadr pair))
            ) ; unless END
          ) ; lambda END
          (param-pairs body)
        ) ; mapcar END
      ) ; lst END
    ) ; variable definition END
    `(mapc #'eval ',lst) ; This list will be the body of function at 'runtime'
  ) ; let END
) ; defmacro mdefvar END
(compile 'mdefvar)
; (describe (macro-function 'mdefvar))
; (documentation 'mdefvar 'function)

;; FINAL VERSIONS:

(fmakunbound 'mdefparameter)

(defmacro mdefparameter (&body body)
"
Defines multiple global variables at once. Variable name must not be NIL.
If there is only 1 variable name provided, that variable's value is set to NIL.
If 'mdefparameter' has odd number of arguments, the last variable's value is set to NIL.
If no arguments are provided, or single NIL argument is provided, does nothing.
If variables with same names are already defined, changes their values.
Examples:
(mdefparameter *foo* (vector 1 2 3) *bar* nil *baz* 'hello *abc* (mapcar #'sqrt '(10 20 30)) *xyz*)
(mdefparameter *foo*)
(macroexpand '(mdefparameter foo 44 bar 77 baz))
"
  (let
    ( ; variable definition START
      (lst ; This variable exists only at 'macro expand time'
        (mapcar
          (lambda (pair)
            (unless (null (car pair))
              (list 'defparameter (car pair) (cadr pair))
            ) ; unless END
          ) ; lambda END
          (param-pairs body)
        ) ; mapcar END
      ) ; lst END
    ) ; variable definition END
    `(progn ,@lst nil) ; This list will be the body of function at 'runtime'
  ) ; let END
) ; defmacro mdefparameter END
(compile 'mdefparameter)
; (describe (macro-function 'mdefparameter))
; (documentation 'mdefparameter 'function)

(fmakunbound 'mdefvar)

(defmacro mdefvar (&body body)
"
Defines multiple global variables at once. Variable name must not be NIL.
If there is only 1 variable name provided, that variable's value is set to NIL.
If 'mdefvar' has odd number of arguments, the last variable's value is set to NIL.
If no arguments are provided, or single NIL argument is provided, does nothing.
If variables with same names are already defined, does not change their values.
Examples:
(mdefvar *foo* (vector 1 2 3) *bar* nil *baz* 'hello *abc* (mapcar #'sqrt '(10 20 30)) *xyz*)
(mdefvar *foo*)
(macroexpand '(mdefvar foo 44 bar 77 baz))
"
  (let
    ( ; variable definition START
      (lst ; This variable exists only at 'macro expand time'
        (mapcar
          (lambda (pair)
            (unless (null (car pair))
              (list 'defvar (car pair) (cadr pair))
            ) ; unless END
          ) ; lambda END
          (param-pairs body)
        ) ; mapcar END
      ) ; lst END
    ) ; variable definition END
    `(progn ,@lst nil) ; This list will be the body of function at 'runtime'
  ) ; let END
) ; defmacro mdefvar END
(compile 'mdefvar)
; (describe (macro-function 'mdefvar))
; (documentation 'mdefvar 'function)

