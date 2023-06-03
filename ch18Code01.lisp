;;;; CHAPTER 18 (Land Of Lisp), PART 1

;; clear screen START
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
;; clear screen END

;;;; Lazy programming, using a feature called 'lazy evaluation'

;; Every time macro 'lazy' is called, anonymous closure is created.
;; It will stay in memory and never be garbage-collected.
(defmacro lazy (&body body)
  (let
    ( ; variable definitions START
      (forced (gensym))
      (value (gensym))
    ) ; variable definitions END
    `(let
      ( ; variable definitions START
        (,forced nil)
        (,value nil)
      ) ; variable definitions END
      (lambda ()
        (unless ,forced
          (setf ,value (progn ,@body))
          (setf ,forced t)
        ) ; unless END
        ,value
      ) ; lambda END
    ) ; let END
  ) ; let END
) ; defmacro lazy END

(defun force (lazy-value)
  (funcall lazy-value)
) ; defun force END

; (lazy (+ 1 2))
; (force (lazy (+ 1 2)))

(defun add (a b)
  (princ "I am adding now")
  (+ a b)
) ; defun add END

; (defparameter *foo* (lazy (add 1 2)))
; (force *foo*)

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d))
) ; defmacro lazy-cons END

(defun lazy-car (x)
  (car (force x))
) ; defun lazy-car END

(defun lazy-cdr (x)
  (cdr (force x))
) ; defun lazy-cdr END

; (defparameter *foo* (lazy-cons 4 7))
; (lazy-car *foo*)
; (lazy-cdr *foo*)

;; Result is (number . anonymous_closure), funcalling anonymous_closure will
;; return (next_number . new_anonymous_closure) every time. For each number there
;; will be new anonymous closure that will stay in memory until end of program.
;; To observe these results in SBCL, run:
;; *integers*
;; (force *integers*)
;; (force (lazy-cdr *integers*))
;; (force (lazy-cdr (lazy-cdr *integers*)))
;; (force (lazy-cdr (lazy-cdr (lazy-cdr *integers*))))
;; (force (lazy-cdr (lazy-cdr (lazy-cdr (lazy-cdr *integers*)))))
(defparameter *integers*
  (labels
    ( ; function definition START
      (f (n)
        (lazy-cons
          n
          (f (1+ n))
        ) ; lazy-cons END
      ) ; f END
    ) ; function definition END
    (f 1)
  ) ; labels END
) ; defparameter *integers* END

; *integers*
; (lazy-car *integers*)
; (force *integers*)
; (lazy-car (lazy-cdr *integers*))
; (force (lazy-cdr *integers*))
; (lazy-car (lazy-cdr (lazy-cdr *integers*)))
; (force (lazy-cdr (lazy-cdr *integers*)))


;; This function will create an anonymous closure every time it
;; is called. The closure will stay in memory and will never be
;; garbage-collected. Use fixed version of this function instead.
; (defun lazy-nil ()
;   (lazy nil)
; ) ; defun lazy-nil END

;; Fixed 'lazy-nil' function
(defun lazy-nil ()
  (lambda ()
    nil
  ) ; lambda END
) ; defun lazy-nil END

(defun lazy-null (x)
  (not (force x))
) ; defun lazy-null END

;; Convert a regular list into a lazy list
(defun make-lazy (lst)
  (lazy
    (when lst
      (cons
        (car lst)
        (make-lazy (cdr lst))
      ) ; cons END
    ) ; when END
  ) ; lazy END
) ; defun make-lazy END

;; Convert a lazy list into a regular list of specified size
(defun take (n lst)
  (unless
    (or
      (zerop n)
      (lazy-null lst)
    ) ; or END
    (cons
      (lazy-car lst)
      (take (1- n) (lazy-cdr lst))
    ) ; cons END
  ) ; unless END
) ; defun take END

;; Convert a lazy list into a regular list
(defun take-all (lst)
  (unless
    (lazy-null lst)
    (cons
      (lazy-car lst)
      (take-all (lazy-cdr lst))
    ) ; cons END
  ) ; unless END
) ; defun take-all END

; (take 200 *integers*)
; (take 10 *integers*)
; (take 10 (make-lazy '(q w e r t y u i o p a s d f)))
; (take-all (make-lazy '(q w e r t y u i o p a s d f)))

;; Mapping and searching across lazy lists
(defun lazy-mapcar (fun lst)
  (lazy
    (unless (lazy-null lst)
      (cons
        (funcall fun (lazy-car lst))
        (lazy-mapcar fun (lazy-cdr lst))
      ) ; cons END
    ) ; unless END
  ) ; lazy END
) ; defun lazy-mapcar END

(defun lazy-mapcan (fun lst)
  (labels
    ( ; function definition START
      (f (lst-cur)
        (if (lazy-null lst-cur)
          (force (lazy-mapcan fun (lazy-cdr lst)))
          (cons
            (lazy-car lst-cur)
            (lazy (f (lazy-cdr lst-cur)))
          ) ; cons END
        ) ; if END
      ) ; f END
    ) ; function definition END
    (lazy
      (unless (lazy-null lst)
        (f (funcall fun (lazy-car lst)))
      ) ; unless END
    ) ; lazy END
  ) ; labels END
) ; defun lazy-mapcan END

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let
      ( ; variable definition START
        (x (lazy-car lst))
      ) ; variable definition END
      (if (funcall fun x)
        x
        (lazy-find-if fun (lazy-cdr lst))
      ) ; if END
    ) ; let END
  ) ; unless END
) ; defun lazy-find-if END

(defun lazy-nth (n lst)
  (if (zerop n)
    (lazy-car lst)
    (lazy-nth (1- n) (lazy-cdr lst))
  ) ; if END
) ; defun lazy-nth END

; (take 10 (lazy-mapcar #'sqrt *integers*))
; (take 10 (lazy-mapcan (lambda (x) (if (evenp x) (make-lazy (list x)) (lazy-nil))) *integers*))
; (lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))
; (lazy-nth 4 (make-lazy '(a b c d e f g)))

