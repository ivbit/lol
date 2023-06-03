;;;; Library of utilities

;; clear screen (Unix-like systems) START
(defun clear ()
 #+clisp
  (ext:run-program "/usr/bin/clear")
 #+sbcl
  (sb-ext:run-program "/usr/bin/clear" nil :input nil :output *standard-output*)
 #+clozure
  (ccl:run-program "/usr/bin/clear" nil :input nil :output *standard-output*)
 '(screen was cleared)
) ; defun clear END
(defun clear0 () (clear))
(defun cls () (clear))
;; clear screen (Unix-like systems) END

;; Flattens a LIST of any complexity. LIST must be a proper list.
(defun flatten (x)
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

;; Applies FUNCTION to respective element(s) of each LIST, appending all
;; the result lists to a single list. FUNCTION must return a list.
(defun mappend (fun &rest lists)
  (loop
    for results in (apply #'mapcar fun lists)
    append results
  ) ; loop END
) ; defun END
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

