;;;; SAFE (non-destructive) VERSION OF 'MAPCAN' FUNCTION

;; clear screen START
(defun clear ()
  #+clisp
    (ext:run-program "/usr/bin/clear")
  #+sbcl
    (sb-ext:run-program "/usr/bin/clear" '() :input nil :output *standard-output*)
  #+clozure
    (ccl:run-program "/usr/bin/clear" '() :input nil :output *standard-output*)
  '(screen was cleared)
) ; defun clear END
(defun clear0 () (clear))
(defun cls () (clear))
;; clear screen END

;; 'mapcan' uses 'nconc' and mangles original list:
(defparameter *foo* '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12)))

(mapcan
  (lambda (x)
    (when (evenp (car x))
      x
    ) ; when END
  ) ; lambda END
  *foo*
) ; mapcan END
*foo*
; ((1) (2 4 6 8 10 12) (3) (4 6 8 10 12) (5) (6 8 10 12) (7) (8 10 12) (9) (10 12) (11) (12))

(defparameter *bar* '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12)))

;; 'safe-mapcan' does not mangle the original list, unlike the 'mapcan' function
(defun safe-mapcan (fun lst)
  (labels
    ( ; function definition START
      (f (returned-lst)
        (if (null returned-lst)
          (safe-mapcan fun (cdr lst))
          (cons
            (car returned-lst)
            (f (cdr returned-lst))
          ) ; cons END
        ) ; if END
      ) ; f END
    ) ; function definition END
    (unless (null lst)
      (f (funcall fun (car lst)))
    ) ; unless END
  ) ; labels END
) ; defun safe-mapcan END

(compile 'safe-mapcan)

(safe-mapcan
  (lambda (x)
    (when (evenp (car x))
      x
    ) ; when END
  ) ; lambda END
  *bar*
) ; safe-mapcan END
*bar*
; ((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12))

(fmakunbound 'safe-mapcan)

;;;; TAIL CALL OPTIMIZATION:

;; In this version the tail call optimization works in Sbcl and Clozure,
;; but not in Clisp. Clisp will run tail call optimization only if function calls
;; itself as a last call. Sbcl and Clozure will run tail call optimization if
;; function calls itself as a last call, OR if function calls other function as a
;; last call. Clisp will fail on big lists with 'Lisp stack overflow. RESET' error.
(defun safe-mapcan (fun lst &optional (acc nil))
  (labels
    ( ; function definition START
      (f (returned-lst acc)
        (if (null returned-lst)
          (safe-mapcan fun (cdr lst) acc)
          (f (cdr returned-lst) (cons (car returned-lst) acc))
        ) ; if END
      ) ; f END
    ) ; function definition END
    (if (null lst)
      (reverse acc)
      (f (funcall fun (car lst)) acc)
    ) ; if END
  ) ; labels END
) ; defun safe-mapcan END

(compile 'safe-mapcan)

(fmakunbound 'safe-mapcan)

;; 'safe-mapcan' does not mangle the original list, unlike the 'mapcan' function
;; The only version where tail call optimization works in Clisp:
(defun safe-mapcan (fun lst)
  (labels
    ( ; function definition START
      (f (lst returned-lst acc)
        (if (null lst)
          (reverse acc)
          (if (null returned-lst)
            (if
              (and
                (null (cadr lst)) ; needed to prevent calculating length every time
                (null (caddr lst)) ; needed to prevent calculating length every time
                (null (cadddr lst)) ; needed to prevent calculating length every time
                (= (length lst) 1) ; check for the end of list
              ) ; and END
              (reverse acc)
              (f (cdr lst) (funcall fun (cadr lst)) acc)
            ) ; if END
            (f lst (cdr returned-lst) (cons (car returned-lst) acc))
          ) ; if END
        ) ; if END
      ) ; f END
    ) ; function definition END
    (unless (null lst) ; 'unless' returns NIL if condition is 'false'
      (f lst (funcall fun (car lst)) nil)
    ) ; unless END
  ) ; labels END
) ; defun safe-mapcan END

;; Need to compile function to enable tail call optimization in Clisp
(compile 'safe-mapcan)

; (safe-mapcan #'list (loop for i below 100000 collect i))

(fmakunbound 'safe-mapcan)

;; Version of 'safe-mapcan' with 'cond' instead of 'if'
;; 'safe-mapcan' does not mangle the original list, unlike the 'mapcan' function
;; The only version where tail call optimization works in Clisp:
(defun safe-mapcan (fun lst)
  (labels
    ( ; function definition START
      (f (lst returned-lst acc)
        (cond
          ( ; 1st condition START
            (or
              (null lst)
              (and
                (null returned-lst)
                (null (cadr lst)) ; needed to prevent calculating length every time
                (null (caddr lst)) ; needed to prevent calculating length every time
                (null (cadddr lst)) ; needed to prevent calculating length every time
                (= (length lst) 1) ; check for the end of list
              ) ; and END
            ) ; or END
            (reverse acc)
          ) ; 1st condition END
          ( ; 2nd condition START
            (null returned-lst)
            (f (cdr lst) (funcall fun (cadr lst)) acc)
          ) ; 2nd condition END
          ( ; default condition START
            t
            (f lst (cdr returned-lst) (cons (car returned-lst) acc))
          ) ; default condition END
        ) ; cond END
      ) ; f END
    ) ; function definition END
    (unless (null lst) ; 'unless' returns NIL if condition is 'false'
      (f lst (funcall fun (car lst)) nil)
    ) ; unless END
  ) ; labels END
) ; defun safe-mapcan END

;; Need to compile function to enable tail call optimization in Clisp
(compile 'safe-mapcan)

; (safe-mapcan #'list (loop for i below 100000 collect i))
; (defparameter *bar* '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12)))
; (safe-mapcan (lambda (x) (when (oddp (car x)) x)) *bar*)
; (safe-mapcan #'print '((1 2 3) nil (foo bar) nil nil (nil) ("hello world" 734 2 5) nil nil nil nil nil (lisp) nil))
;; Original 'mapcan' will hang while running this, 'safe-mapcan' have no problem doing so:
; (safe-mapcan (lambda (i) (if (evenp i) (list (* i i i)) '(a b c))) '(1 2 3 4 5 6 7 8 9 0 11 12 13))

(fmakunbound 'safe-mapcan)

;;;; Improved version of 'safe-mapcan':

;; 'mapcan' runs a function for every element of a list, expects that function to return
;; list every time, destructively concatenates those lists using 'nconc'.
;; 'safe-mapcan' does not mangle the original list, unlike the 'mapcan' function.
;; The only version where tail call optimization works in Clisp:
;; 'fun' returns list (evaluates to list), 'safe-mapcan' concatenates those lists
(defun safe-mapcan (fun lst)
  (labels
    ( ; function definition START
      (f (lst returned-lst acc) ; variable shadowing: same name 'lst' in 'safe-mapcan', 'f'
        (if
          (or
            (null lst)
            (and
              (null returned-lst)
              (null (cadr lst)) ; needed to prevent calculating length every time
              (null (caddr lst)) ; needed to prevent calculating length every time
              (null (cadddr lst)) ; needed to prevent calculating length every time
              (= (length lst) 1) ; check for the end of list
            ) ; and END
          ) ; or END
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
) ; defun safe-mapcan END

;; Need to compile function to enable tail call optimization in Clisp
(compile 'safe-mapcan)

; (safe-mapcan #'list (loop for i below 100000 collect i))
; (defparameter *bar* '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12)))
; (safe-mapcan (lambda (x) (when (oddp (car x)) x)) *bar*)
; (safe-mapcan #'print '((1 2 3) nil (foo bar) nil nil (nil) ("hello world" 734 2 5) nil nil nil nil nil (lisp) nil))
;; Original 'mapcan' will hang while running this, 'safe-mapcan' have no problem doing so:
; (safe-mapcan (lambda (i) (if (evenp i) (list (* i i i)) '(a b c))) '(1 2 3 4 5 6 7 8 9 0 11 12 13))

(fmakunbound 'safe-mapcan)

;;;; FINAL VERSION of 'safe-mapcan':

;; 'mapcan' runs a function for every element of a list, expects that function to return
;; list every time, destructively concatenates those lists using 'nconc'.
;; 'safe-mapcan' does not mangle the original list, unlike the 'mapcan' function.
;; The only version where tail call optimization works in Clisp:
;; 'fun' returns list (evaluates to list), 'safe-mapcan' concatenates those lists
(defun safe-mapcan (fun lst)
  (labels
    ( ; function definition START
      (f (lst returned-lst acc) ; variable shadowing: same name 'lst' in 'safe-mapcan', 'f'
        (if
          (and
            (null returned-lst)
            (null (cadr lst)) ; needed to prevent calculating length every time
            (null (caddr lst)) ; needed to prevent calculating length every time
            (null (cadddr lst)) ; needed to prevent calculating length every time
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
) ; defun safe-mapcan END

;; Need to compile function to enable tail call optimization in Clisp
(compile 'safe-mapcan)

; (safe-mapcan #'list (loop for i below 100000 collect i))
; (defparameter *bar* '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12)))
; (safe-mapcan (lambda (x) (when (oddp (car x)) x)) *bar*)
; (safe-mapcan #'print '((1 2 3) nil (foo bar) nil nil (nil) ("hello world" 734 2 5) nil nil nil nil nil (lisp) nil))
;; Original 'mapcan' will hang while running this, 'safe-mapcan' have no problem doing so:
; (safe-mapcan (lambda (i) (if (evenp i) (list (* i i i)) '(a b c))) '(1 2 3 4 5 6 7 8 9 0 11 12 13))

