;;;; CHAPTER 17 (Land Of Lisp), PART 1

;; clear screen START
(defun clear ()
  #+clisp
    (ext:run-program "/usr/bin/clear")
  #+sbcl
    (sb-ext:run-program "/usr/bin/clear" '() :input '() :output *standard-output*)
  #+clozure
    (ccl:run-program "/usr/bin/clear" '() :input '() :output *standard-output*)
  '(screen was cleared)
) ; defun clear END
(defun clear0 () (clear))
(defun cls () (clear))
;; clear screen END

;;;; Domain-specific languages

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

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/)
  ) ; when END
  (princ (string-downcase name))
  (mapc
    (lambda (att)
      (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att))
    ) ; lambda END
    alst
  ) ; mapc END
  (princ #\>)
) ; defun print-tag END

(defmacro tag (name atts &body body)
  `(progn
    (print-tag
      ',name
      (list
        ,@(mapcar
          (lambda (x)
            `(cons ',(car x) ,(cdr x))
          ) ; lambda END
          (pairs atts)
        ) ; mapcar END
      ) ; list END
      nil
    ) ; print-tag END
    ,@body
    (print-tag ',name nil t)
  ) ; progn END
) ; defmacro tag END

; (tag mytag (color 'blue height (+ 4 5)))
; (macroexpand '(tag mytag (color 'blue height (+ 4 5))))
; (progn (print-tag 'mytag (list (cons 'color 'blue) (cons 'height (+ 4 5))) nil) (print-tag 'mytag nil t))
; (tag mytag (color 'blue size 'big) (tag 1st_inner_tag nil) (tag 2nd_inner_tag nil))
; (tag html nil (tag body nil (princ "Hello World!")))

(defmacro html (&body body)
  `(tag
    html
    nil
    ,@body
  ) ; tag END
) ; defmacro html END

(defmacro body (&body body)
  `(tag
    body
    nil
    ,@body
  ) ; tag END
) ; defmacro body END

; (html (body (princ "Hello World!")))

(defmacro svg (width height &body body)
  `(tag
    svg
    ( ; attribute pairs START
      xmlns "http://www.w3.org/2000/svg"
      "xmlns:xlink" "http://www.w3.org/1999/xlink"
      height ,height
      width ,width
    ) ; attribute pairs END
    ,@body
  ) ; tag END
) ; defmacro svg END

(defun brightness (col amt)
  (mapcar
    (lambda (x)
      (min
        255
        (max 0 (+ x amt))
      ) ; min END
    ) ; lambda END
    col
  ) ; mapcar END
) ; defun brightness END

; (brightness '(255 225 205) -100)

(defun svg-style (color)
  (format
    nil
    "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
    (append
      color
      (brightness color -100)
    ) ; append END
  ) ; format END
) ; defun svg-style END

(defun circle (center radius color)
  (tag
    circle
    ( ; attribute pairs START
      cx (car center)
      cy (cdr center)
      r radius
      style (svg-style color)
    ) ; attribute pairs END
  ) ; tag END
) ; defun circle END

; (svg (circle '(50 . 50) 50 '(255 0 0)) (circle '(100 . 100) 50 '(0 0 255)))

(defun polygon (points color)
  (tag
    polygon
    ( ; attribute pairs START
      points (format
        nil
        "~{~a,~a ~}"
        (mapcan
          (lambda (tp)
            (list (car tp) (cdr tp))
          ) ; lambda END
          points
        ) ; mapcan END
      ) ; format END
      style (svg-style color)
    ) ; attribute pairs START
  ) ; tag END
) ; defun polygon END

(defun random-walk (value length)
  (unless (zerop length)
    (cons
      value
      (random-walk
        (if (zerop (random 2))
          (1- value)
          (1+ value)
        ) ; if END
        (1- length)
      ) ; random-walk END
    ) ; cons END
  ) ; unless END
) ; defun random_walk END

