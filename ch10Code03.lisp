;;;; CHAPTER 10 (Land Of Lisp)

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

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)

(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  (let
    ( ; variable definitions START
      (pos (cons (+ left (random width)) (+ top (random height))))
    ); variable definitions END
    (setf (gethash pos *plants*) t)
  ) ; let END
) ; defun random-plant END

; using apply to supply a list as arguments to a function
(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*)
) ; defun add-plants END

(defstruct animal x y energy dir genes)

(defparameter *animals*
  (list
    (make-animal
      :x      (ash *width* -1)
      :y      (ash *height* -1)
      :energy 1000
      :dir    0
      :genes  (loop repeat 8 collecting (1+ (random 10)))
    ) ; make-animal END
  ) ; list END
) ; defparameter *animals* END

(defun move (animal)
  (let
    ( ; variable definitions START
      (dir (animal-dir animal))
      (x (animal-x animal))
      (y (animal-y animal))
    ) ; variable definitions END
    (setf
      (animal-x animal)
      (mod
        (+
          x
          (cond
            ((and (>= dir 2) (< dir 5)) 1)
            ((or (= dir 1) (= dir 5)) 0)
            (t -1)
          ) ; cond END
          *width*
        ) ; + END
        *width*
      ) ; mod END
    ) ; setf END
    (setf
      (animal-y animal)
      (mod
        (+
          y
          (cond
            ((and (>= dir 0) (< dir 3)) -1)
            ((and (>= dir 4) (< dir 7)) 1)
            (t 0)
          ) ; cond END
          *height*
        ) ; + END
        *height*
      ) ; mod END
    ) ; setf END
    (decf (animal-energy animal))
  ) ; let END
) ; defun move END

(defun turn (animal)
  (let
    ( ; variable definitions START
      (x (random (apply #'+ (animal-genes animal))))
    ) ; variable definitions END
    (labels
      ( ; function definitions START
        (angle (genes x)
          (let
            ( ; variable definitions START
              (xnu (- x (car genes)))
            ) ; variable definitions END
            (if
              (< xnu 0)
              0
              (1+ (angle (cdr genes) xnu))
            ) ; if END
          ) ; let END
        ) ; angle END
      ) ; function definitions END
      (setf
        (animal-dir animal)
        (mod
          (+
            (animal-dir animal)
            (angle (animal-genes animal) x)
          ) ; + END
          8
        ) ; mod END
      ) ; setf END
    ) ; labels END
  ) ; let END
) ; defun turn END

(defun eat (animal)
  (let
    ( ; variable definitions START
      (pos (cons (animal-x animal) (animal-y animal)))
    ) ; variable definitions END
    (when
      (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*)
    ) ; when END
  ) ; let END
) ; defun eat END

(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  (let
    ( ; variable definitions START
      (e (animal-energy animal))
    ) ; variable definitions END
    (when
      (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let
        ( ; variable definitions START
          (animal-nu (copy-structure animal))
          (genes (copy-list (animal-genes animal)))
          (mutation (random 8))
        ) ; variable definitions END
        (setf
          (nth mutation genes)
          (max
            1
            (+ (nth mutation genes) (random 3) -1)
          ) ; max END
        ) ; setf END
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)
      ) ; let END
    ) ; when END
  ) ; let END
) ; defun reproduce END

(defun update-world ()
  (setf
    *animals*
    (remove-if
      (lambda (animal)
        (<= (animal-energy animal) 0)
      ) ; lambda END
      *animals*
    ) ; remove-if END
  ) ; setf END
  (mapc
    (lambda (animal)
      (turn animal)
      (move animal)
      (eat animal)
      (reproduce animal)
    ) ; lambda END
    *animals*
  ) ; mapc END
  (add-plants)
) ; defun update-world END

(defun draw-world ()
  (loop
    for y
    below *height*
    do
      (progn
        (fresh-line)
        (princ "|")
        (loop
          for x
          below *width*
          do
            (princ
              (cond
                ( ; 1st condition START
                  (some
                    (lambda (animal)
                      (and
                        (= (animal-x animal) x)
                        (= (animal-y animal) y)
                      ) ; and END
                    ) ; lambda END
                    *animals*
                  ) ; some END
                  #\M
                ) ; 1st condition END
                ( ; 2nd condition START
                  (gethash
                    (cons x y)
                    *plants*
                  ) ; gethash END
                  #\*
                ) ; 2nd condition END
                ( ; default condition START
                  t
                  #\Space
                ) ; default condition END
              ) ; cond END
            ) ; princ END
        ) ; loop END
        (princ "|")
      ) ; progn END
  ) ; loop END
) ; defun draw-world END

(defun evolution ()
  (clear)
  (draw-world)
  (fresh-line)
  (let
    ( ; variable definitions START
      (str (read-line))
    ) ; variable definitions END
    (cond
      ( ; 1st condition START
        (equal str "quit")
        '()
      ) ; 1st condition END
      ( ; default condition START
        t
        (let
          ( ; variable definitions START
            (x (parse-integer str :junk-allowed t))
          ) ; variable definitions END
          (if
            x
            (loop
              for i
              below x
              do (update-world)
              if
                (zerop (mod i 1000))
                do (princ #\.)
              do (finish-output)
            ) ; loop END
            (update-world)
          ) ; if END
          (evolution)
        ) ; let END
      ) ; default condition END
    ) ; cond END
  ) ; let END
) ; defun evolution END

