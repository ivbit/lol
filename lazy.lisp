;;;; CHAPTER 18 (Land Of Lisp), PART 1
;;;; Library of lazy macros and functions

;;;; Lazy programming, using a feature called 'lazy evaluation'

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

;; Conver a regular list into a lazy list
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

(defun take-all (lst)
  (unless
    (lazy-null lst)
    (cons
      (lazy-car lst)
      (take-all (lazy-cdr lst))
    ) ; cons END
  ) ; unless END
) ; defun take-all END

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

; (lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))
; (lazy-nth 4 (make-lazy '(a b c d e f g)))

