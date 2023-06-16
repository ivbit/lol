;;;; Library of utilities

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

(defun flatten (x)
"
Flattens a LIST of any complexity. LIST must be a proper list.
"
  (labels
    ( ; function definition START
      (rec (x acc)
        (cond
          ( ; 1st condition START
            (null x)
            acc
          ) ; 1st condition END
          ( ; 2nd condition START
            (atom x)
            (cons x acc)
          ) ; 2nd condition END
          ( ; default condition START
            t
            (rec
              (car x)
              (rec (cdr x) acc)
            ) ; rec END
          ) ; default condition END
        ) ; cond END
      ) ; rec END
    ) ; function definition END
    (rec x nil)
  ) ; labels END
) ; defun flatten END
(compile 'flatten)
; (flatten '((1 2 3) (foo (bar baz)) nil (((abc) xyz)) ((nil)) ((((z))))))

(defun mappend (fun &rest lists)
"
Applies FUNCTION to respective element(s) of each LIST, appending all
the result lists to a single list. FUNCTION must return a list.
"
  (loop
    for results in (apply #'mapcar fun lists)
    append results
  ) ; loop END
) ; defun mappend END
(compile 'mappend)
; (defparameter *bar* '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12)))
; (mappend (lambda (x) (when (oddp (car x)) x)) *bar*)

;; 'mapcan' runs a function for every element of a LIST, expects that
;; function to return list every time, destructively concatenates those
;; lists using 'nconc'. 'mapcan' accepts multiple LISTS as &rest arguments.
;; 'smapcan' does not mangle the original LIST, unlike the 'mapcan' function.
;; 'smapcan' accepts a single LIST as 2nd argument.
;; The only version of 'smapcan' where tail call optimization works in Clisp:
;; 'fun' returns list (evaluates to list), 'smapcan' concatenates those lists
(defun smapcan (fun lst)
"
'Safe mapcan' is just like regular 'mapcan', but only accepts a single LIST
and does not mangle the original LIST. Regular 'mapcan' uses 'nconc'.
"
  (labels
    ( ; function definition START
      (f (lst returned-lst acc) ; variable shadowing: variable name 'lst'
        (if
          (and
            (null returned-lst)
            (null (cadr lst)) ; prevent calculating length every time
            (null (caddr lst)) ; prevent calculating length every time
            (null (cadddr lst)) ; prevent calculating length every time
            (= (length lst) 1) ; check for the end of list
          ) ; and END
          (reverse acc)
          (if (null returned-lst)
            (f (cdr lst) (funcall fun (cadr lst)) acc)
            (f lst (cdr returned-lst) (cons (car returned-lst) acc))
          ) ; if END
        ) ; if END
      ) ; f END
    ) ; function definition END
    (unless (null lst) ; 'unless' returns NIL if condition is 'false'
      (f lst (funcall fun (car lst)) nil)
    ) ; unless END
  ) ; labels END
) ; defun smapcan END
;; Need to compile function to enable tail call optimization in Clisp
(compile 'smapcan)
; (defparameter *bar* '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12)))
; (smapcan (lambda (x) (when (oddp (car x)) x)) *bar*)

;; 'trand' returns a true random number, 'bit-amt' can be set to higher
;; or lower values to affect the size of a true random number (trand 128)
;; allowed values are powers of 2 starting from 8:
;; 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8096, ..., 65536, ...
;; (loop for i from 3 upto 20 collect (expt 2 i))
;; 64 is the limit for 'Clozure common lisp' implementation
;; 1024 is the limit for 'Sbcl' implementation
(defun trand (&optional (bit-amt 64))
"
Returns a true random number on Unix-like systems.
'bit-amt' controls the size of a true random number,
allowed values are powers of 2 starting from 8: 8, 16, 32, 64, 128, ...
CCL has limit of 64, SBCL has limit of 1024.
"
  (with-open-file
    ( ; parameter definitions START
      urandom "/dev/urandom"
      :direction :input
      :element-type (list 'unsigned-byte bit-amt)
    ) ; parameter definitions END
    (let*
      ( ; variable definition START
        (temp-array (make-array 1 :element-type (list 'unsigned-byte bit-amt)))
      ) ; variable definition END
      (read-sequence temp-array urandom)
      (aref temp-array 0)
    ) ; let* END
  ) ; with-open-file END
) ; defun trand END
(compile 'trand)
;; get true random number from 1 upto 200
; (1+ (mod (trand 8) 200))
;; get true random number from 0 upto 4500
; (mod (trand 16) 4501)

