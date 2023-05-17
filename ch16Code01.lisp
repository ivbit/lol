;;;; CHAPTER 16 (Land Of Lisp)

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

;;;; Macros

;; &body, &rest puts the rest of values into a list - ((+ 2 3))
(defmacro let1 (var val &body body)
  `(let
     ( ; variable definition START
       (,var ,val)
     ) ; variable definition END
     ; ,@ - take values out of list - (+ 2 3)
     ; without ,@ - ((+ 2 3))
     ,@body
   ) ; let END
) ; defmacro let1 END

; (let1  foo (+ 2 3)   (* foo foo))
; (let ((foo (+ 2 3))) (* foo foo))

; (macroexpand '(let1 foo (+ 2 3) (* foo foo)))

;; anaphoric macro - makes named variables 'head' & 'tail' available to use in body of macro
(defmacro split (val yes no)
  ; this let1 runs at 'macro expand time', it's safe to define variable 'g'
  (let1
    g (gensym)
    ; this let1 runs at 'runtime', 'gensym' is needed to create unique variable name
    `(let1
      ,g ,val
      (if ,g
        (let
          ( ; variable definitions START
            (head (car ,g))
            (tail (cdr ,g))
          ) ; variable definitions END
          ,yes
        ) ; let END
        ,no
      ) ; if END
    ) ; let1 END
  ) ; let1 END
) ; defmacro split END

; (let1 g 100 (split '(2 3) (+ g head) nil))
; (macroexpand '(split '(2 3) (+ g head) nil))
; (let ((#:G3 '(2 3))) (if #:G3 (let ((head (car #:G3)) (tail (cdr #:G3))) (+ g head)) nil))

(defun pairs (lst)
  (labels
    ( ; function definition START
      (f (lst acc)
        (split
          lst
          (if tail
            (f
              (cdr tail)
              (cons
                (cons head (car tail))
                acc
              ) ; cons END
            ) ; f END
            (reverse acc)
          ) ; if END
          (reverse acc)
        ) ; split END
      ) ; f END
    ) ; function definition END
    (f lst nil)
  ) ; labels END
) ; defun pairs END

; (pairs '(a b c d e f))

;; anaphoric macro - makes named function 'self' available to use in body of macro
(defmacro recurse (vars &body body)
  (let1
    p (pairs vars)
    `(labels
      ( ; function definition START
        (self ,(mapcar #'car p)
          ,@body)
      ) ; function definition END
      (self ,@(mapcar #'cdr p))
    ) ; labels END
  ) ; let1 END
) ; defmacro recurse END

; (recurse (n 9) (fresh-line) (if (zerop n) (princ "Done") (progn (princ n) (self (1- n)))))

;; variable shadowing is used for 'lst' inside 'self' function, macroexpanded - (self lst 0)
(defun my-length (lst)
  (recurse
    (lst lst acc 0)
    (split
      lst
      (self tail (1+ acc))
      acc
    ) ; split END
  ) ; recurse END
) ; defun my-length END

; (macroexpand '(recurse (lst lst acc 0) (split lst (self tail (1+ acc)) acc)))
; (labels ((self (lst acc) (split lst (self tail (1+ acc)) acc))) (self lst 0))

;; functional programming version of 'my-length'
(defun my-length (lst)
  (reduce
    (lambda (counter item)
      (1+ counter)
    ) ; lambda END
    lst
    :initial-value 0
  ) ; reduce END
) ; defun my-length END

;; Suppress warning 'The variable is defined but never used'
(defun my-length (lst)
  (reduce
    (lambda (counter item)
      (declare (ignore item))
      (1+ counter)
    ) ; lambda END
    lst
    :initial-value 0
  ) ; reduce END
) ; defun my-length END

