;;;; CHAPTER 18 (Land Of Lisp), PART 2

;;;; Lazy programming, using a feature called 'lazy evaluation'
;;;; Improved 'Dice of doom' game
;;;; Numbers and characters are compared with 'eql', not with 'eq'!

(load "dice_of_doom_v1.lisp")
(load "lazy.lisp")

(makunbound '*board-size*)
(makunbound '*board-hexnum*)
(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; Redefining some of the functions
;; Making list of moves lazy
(fmakunbound 'add-passing-move)
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (lazy-cons
      (list
        nil
        (game-tree
          (add-new-dice board player (1- spare-dice))
          (mod (1+ player) *num-players*)
          0
          t
        ) ; game-tree END
      ) ; list END
      moves
    ) ; lazy-cons END
  ) ; if END
) ; defun add-passing-move END

(fmakunbound 'attacking-moves)
(defun attacking-moves (board cur-player spare-dice)
  (labels
    ( ; function definitions START
      (player (pos) (car (aref board pos)))
      (dice (pos) (cadr (aref board pos)))
    ) ; function definitions END
    (lazy-mapcan
      (lambda (src)
        (if (eql (player src) cur-player)
          (lazy-mapcan
            (lambda (dst)
              (if
                (and
                  (not (eql (player dst) cur-player))
                  (> (dice src) (dice dst))
                ) ; and END
                (make-lazy
                  (list
                    (list
                      (list src dst)
                      (game-tree
                        (board-attack board cur-player src dst (dice src))
                        cur-player
                        (+ spare-dice (dice dst))
                        nil
                      ) ; game-tree END
                    ) ; list END
                  ) ; list END
                ) ; make-lazy END
                (lazy-nil) ; else
              ) ; if END
            ) ; lambda END
            (make-lazy (neighbors src))
          ) ; lazy-mapcan END
          (lazy-nil) ; else
        ) ; if END
      ) ; lambda END
      (make-lazy (loop for n below *board-hexnum* collect n))
    ) ; lazy-mapcan END
  ) ; labels END
) ; defun attacking-moves END

(fmakunbound 'handle-human)
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let
    ( ; variable definition START
      (moves (caddr tree))
    ) ; variable definition END
    (labels
      ( ; function definition START
        (print-moves (moves n)
          (unless (lazy-null moves)
            (let*
              ( ; variable definitions START
                (move (lazy-car moves))
                (action (car move))
              ) ; variable definitions END
              (fresh-line)
              (format t "~a. " n)
              (if action
                (format t "~a -> ~a" (car action) (cadr action))
                (princ "end turn")
              ) ; if END
            ) ; let* END
            (print-moves (lazy-cdr moves) (1+ n))
          ) ; unless END
        ) ; print-moves END
      ) ; function definition END
      (print-moves moves 1)
    ) ; labels END
    (fresh-line)
    (finish-output)
    (cadr (lazy-nth (1- (read)) moves)) ; returns a game tree
  ) ; let END
) ; defun handle-human END

(fmakunbound 'play-vs-human)
(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))
  ) ; if END
) ; defun play-vs-human END

; (play-vs-human (game-tree (gen-board) 0 0 t))

;; Improving AI code
;; Return a new game tree with less moves
(defun limit-tree-depth (tree depth)
  (list
    (car tree) ; player
    (cadr tree) ; board
    (if (zerop depth)
      (lazy-nil)
      (lazy-mapcar ; same result as (caddr tree); subbranches removed below 'depth'
        (lambda (move)
          (list
            (car move)
            (limit-tree-depth (cadr move) (1- depth))
          ) ; list END
        ) ; lambda END
        (caddr tree) ; '(((1stMove)(tree)) ((2ndMove)(tree)) ...)
      ) ; lazy-mapcar END
    ) ; if END
  ) ; list END
) ; defun limit-tree-depth END

(defparameter *ai-level* 4)
(fmakunbound 'handle-computer)
(defun handle-computer (tree)
  (let
    ( ; variable definition START
      (ratings
        (get-ratings
          (limit-tree-depth tree *ai-level*)
          (car tree)
        ) ; get-ratings END
      ) ; ratings END
    ) ; variable definition END
    (cadr
      (lazy-nth
        (position (apply #'max ratings) ratings)
        (caddr tree)
      ) ; lazy-nth END
    ) ; cadr END
  ) ; let END
) ; defun handle-computer END

(fmakunbound 'play-vs-computer)
(defun play-vs-computer (tree)
  (print-info tree)
  (cond
    ( ; 1st condition START
      (lazy-null (caddr tree)) ; list of moves is empty
      (announce-winner (cadr tree))
    ) ; 1st condition END
    ( ; 2nd condition START
      (zerop (car tree))
      (play-vs-computer (handle-human tree))
    ) ; 2nd condition END
    ( ; default condition START
      t
      (play-vs-computer (handle-computer tree))
    ) ; default condition END
  ) ; cond END
) ; defun play-vs-computer END

;; Applying heuristics
;; Heuristic - fast technique, but not guaranteed to work 100% of the time
(defun score-board (board player)
  (loop
    for hex across board
    for pos from 0
    sum
      (if (eql (car hex) player)
        (if (threatened pos board)
          1
          2
        ) ; if END
        -1
      ) ; if END
  ) ; loop END
) ; defun score-board END

(defun threatened (pos board)
  (let*
    ( ; variable definitions START
      (hex (aref board pos))
      (player (car hex))
      (dice (cadr hex))
    ) ; variable definitions END
    (loop
      for n in (neighbors pos)
      do
        (let*
          ( ; variable definitions START
            (nhex (aref board n))
            (nplayer (car nhex))
            (ndice (cadr nhex))
          ) ; variable definitions END
          (when
            (and
              (not (eql player nplayer))
              (> ndice dice)
            ) ; and END
            (return t) ; exit from loop
          ) ; when END
        ) ; let* END
    ) ; loop END
  ) ; let* END
) ; defun threatened END

(fmakunbound 'get-ratings)
(defun get-ratings (tree player)
  (take-all
    (lazy-mapcar
      (lambda (move)
        (rate-position (cadr move) player)
      ) ; lambda END
      (caddr tree)
    ) ; lazy-mapcar END
  ) ; take-all END
) ; defun get-ratings END

(fmakunbound 'rate-position)
(defun rate-position (tree player)
  (let
    ( ; variable definition START
      (moves (caddr tree))
    ) ; variable definition END
    (if (not (lazy-null moves))
      (apply
        (if (eql (car tree) player)
          #'max
          #'min
        ) ; if END
        (get-ratings tree player)
      ) ; apply END
      (score-board (cadr tree) player) ; else
    ) ; if END
  ) ; let END
) ; defun rate-position END

; (play-vs-computer (game-tree (gen-board) 0 0 t))

;; Alpha-beta pruning
;; Rewriting 'get-ratings' as 'ab-get-ratings-max' and 'ab-get-ratings-min'
(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels
    ( ; function definition START
      (f (moves lower-limit)
        (unless (lazy-null moves)
          (let
            ( ; variable definition START
              (x
                (ab-rate-position
                  (cadr (lazy-car moves))
                  player
                  upper-limit
                  lower-limit
                ) ; ab-rate-position END
              ) ; x END
            ) ; variable definition END
            (if (>= x upper-limit)
              (list x)
              (cons
                x
                (f (lazy-cdr moves) (max x lower-limit))
              ) ; cons END
            ) ; if END
          ) ; let END
        ) ; unless END
      ) ; f END
    ) ; function definition END
    (f (caddr tree) lower-limit)
  ) ; labels END
) ; defun ab-get-ratings-max END

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels
    ( ; function definition START
      (f (moves upper-limit)
        (unless (lazy-null moves)
          (let
            ( ; variable definition START
              (x
                (ab-rate-position
                  (cadr (lazy-car moves))
                  player
                  upper-limit
                  lower-limit
                ) ; ab-rate-position END
              ) ; x END
            ) ; variable definition END
            (if (<= x lower-limit)
              (list x)
              (cons
                x
                (f (lazy-cdr moves) (min x upper-limit))
              ) ; cons END
            ) ; if END
          ) ; let END
        ) ; unless END
      ) ; f END
    ) ; function definition END
    (f (caddr tree) upper-limit)
  ) ; labels END
) ; defun ab-get-ratings-min END

;; function that rates a single board arrangement
;; 'ab-rate-position' will be used instead of 'rate-position'
(defun ab-rate-position (tree player upper-limit lower-limit)
  (let
    ( ; variable definition START
      (moves (caddr tree))
    ) ; variable definition END
    (if (not (lazy-null moves))
      (if (eql (car tree) player)
        (apply
          #'max
          (ab-get-ratings-max
            tree
            player
            upper-limit
            lower-limit
          ) ; ab-get-ratings-max END
        ) ; apply END
        (apply
          #'min
          (ab-get-ratings-min
            tree
            player
            upper-limit
            lower-limit
          ) ; ab-get-ratings-min END
        ) ; apply END
      ) ; if END
      (score-board (cadr tree) player) ; else
    ) ; if END
  ) ; let END
) ; defun ab-rate-position END


(fmakunbound 'handle-computer)
(defun handle-computer (tree)
  (let
    ( ; variable definition START
      (ratings
        (ab-get-ratings-max
          (limit-tree-depth tree *ai-level*)
          (car tree)
          most-positive-fixnum
          most-negative-fixnum
        ) ; ab-get-ratings-max END
      ) ; ratings END
    ) ; variable definition END
    (cadr
      (lazy-nth
        (position (apply #'max ratings) ratings)
        (caddr tree)
      ) ; lazy-nth END
    ) ; cadr END
  ) ; let END
) ; defun handle-computer END


(makunbound '*board-size*)
(makunbound '*board-hexnum*)
(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))

; (play-vs-computer (game-tree (gen-board) 0 0 t))

