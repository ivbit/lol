;;;; CHAPTER 9 (Land Of Lisp)

;; clear screen START
(defun clear ()
  #+clisp
    (ext:run-program "/usr/bin/clear")
  #+sbcl 
    (sb-ext:run-program "/usr/bin/clear" 'nil :input 'nil :output *standard-output*)
  #+clozure 
    (ccl:run-program "/usr/bin/clear" 'nil :input 'nil :output *standard-output*)
  '(screen was cleared.)
) ; defun clear END
(defun clear0 () (clear))
(defun cls () (clear))
;; clear screen END

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over.")
  ) ; when (player-dead) END
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")
  ) ; when (monsters-dead) END
) ; defun orc-battle END

(defun game-loop ()
  (unless
    (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes
      (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless
        (monsters-dead)
        (show-monsters)
        (player-attack)
      ) ; unless END
    ) ; dotimes END
    (fresh-line)
    (map 'list
    (lambda (m)
      (or (monster-dead m) (monster-attack m))
    ) ; lambda END
    *monsters*)
    (game-loop)
  ) ; unless END
) ; defun game-loop END

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30)
) ; defun init-player END

(defun player-dead ()
  (<= *player-health* 0)
) ; defun player-dead END

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (print *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*)
) ; defun show-player END

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (finish-output)
  (case
    (read)
    (s
      (let
        ((x (+ 2 (randval (ash *player-strength* -1)))))
        (princ "Your stab has a strength of ")
        (princ x)
        (fresh-line)
        (monster-hit (pick-monster) x)
      ) ; let END
    ) ; s END
    (d
      (let
        ((x (randval (truncate (/ *player-strength* 6)))))
        (princ "Your double swing has a strength of ")
        (princ x)
        (fresh-line)
        (monster-hit (pick-monster) x)
        (unless (monsters-dead)
          (monster-hit (pick-monster) x)
        ) ; unless END
      ) ; let END
    ) ; d END
    (otherwise
      (dotimes
        (x (1+ (randval (truncate (/ *player-strength* 3)))))
        (unless
          (monsters-dead)
          (monster-hit (random-monster) 1)
        ) ; unless END
      ) ; dotimes END
    ) ; otherwise END
  ) ; case END
) ; defun player-attack END

(defun randval (n)
  (1+ (random (max 1 n)))
) ; defun randval END

(defun random-monster ()
  (let
    ((m (aref *monsters* (random (length *monsters*)))))
    (if
      (monster-dead m)
      (random-monster)
      m
    ) ; if END
  ) ; let END
) ; defun random-monster END

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #: ")
  (finish-output)
  (let
    ((x (read)))
    (if
      (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
      (progn
        (princ "That is not a valid monster number.")
        (pick-monster)
      ) ; progn END
      (let
        ((m (aref *monsters* (1- x))))
        (if
          (monster-dead m)
          (progn
            (princ "That monster is already dead.")
            (pick-monster)
          ) ; progn END
          m
        ) ; if END
      ) ; let END
    ) ; if END
  ) ; let END
) ; defun pick-monster END

(defun init-monsters ()
  (setf *monsters*
    (map 'vector
      (lambda (x)
        (funcall (nth (random (length *monster-builders*)) *monster-builders*))
      ) ; lambda END
      (make-array *monster-num*)
    ) ; map END
  ) ; setf *monsters* END
) ; defun init-monsters

(defun monster-dead (m)
  (<= (monster-health m) 0)
) ; defun monster-dead END

(defun monsters-dead ()
  (every #'monster-dead *monsters*)
) ; defun monster-dead END

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let
    ((x 0))
    (map 'list
      (lambda (m)
        (fresh-line)
        (princ "   ")
        (princ (incf x))
        (princ ". ")
        (if
          (monster-dead m)
          (princ "**dead**")
          (progn
            (princ "(Health=")
            (princ (monster-health m))
            (princ ") " )
            (monster-show m)
          ) ; progn END
        ) ; if END
      ) ; lambda END
      *monsters*
    ) ; map END
  ) ; let END
) ; defun show-monsters END

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if
    (monster-dead m)
    (progn
      (princ "You killed the ")
      (princ (type-of m))
      (princ "! ")
    ) ; progn END
    (progn
      (princ "You hit the ")
      (princ (type-of m))
      (princ ", knocking off ")
      (princ x)
      (princ " health points! ")
    ) ; progn END
  ) ; if END
) ; defmethod monster-hit END

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m))
) ; defmethod monster-show END

(defmethod monster-attack (m))

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club")
) ; defmethod monster-show

(defmethod monster-attack ((m orc))
  (let
    ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)
    ) ; let END
) ; defmethod monster-attack END

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads.")
) ; defmethod monster-show END

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
    (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
    (progn
      (princ "You lop off ")
      (princ x)
      (princ " of the hydra's heads! ")
    ) ; progn END
  ) ; if END
) ; defmethod monster-hit END

(defmethod monster-attack ((m hydra))
  (let
    ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attack you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)
  ) ; let END
) ; defmethod monster-attack END

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m))
) ; defmethod monster-show END

(defmethod monster-attack ((m slime-mold))
  (let
    ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when
      (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*)
    ) ; when END
  ) ; let END
) ; defmethod monster-attack END

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let
    ((x (max *player-health* *player-agility* *player-strength*)))
    (cond
      ( ; 1st condition START
        (= x *player-health*)
        (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
        (decf *player-health* 2)
      ) ; 1st condition END
      ( ; 2nd condition START
        (= x *player-agility*)
        (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
        (decf *player-agility* 2)
      ) ; 2nd condition END
      ( ; 3rd condition START
        (= x *player-strength*)
        (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
        (decf *player-strength* 2)
      ) ; 3rd condition END
    ) ; cond END
  ) ; let END
) ; defmethod END

