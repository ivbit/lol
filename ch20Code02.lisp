;;;; CHAPTER 20 (Land Of Lisp)

;; Script works in Clisp, Sbcl and Clozure common lisp implementations.
;; Sbcl is fastest and provide the best gaming experience for a player.
;; ccl64 -l ~/lisp/lol/ch20/ch20Code02.lisp -e "(serve #'dod-request-handler)"

(load "dice_of_doom_v3.lisp")

(fmakunbound 'player-name)
(defun player-name (n)
  (if (zerop n)
    "<strong>HUMAN</strong>"
    (format nil "<strong>COMPUTER-~a</strong>" (code-char (+ n 65))) ; else
  ) ; if END
) ; defun player-name END

(makunbound '*num-players*)
(defparameter *num-players* 4)
(makunbound '*die-colors*)
(defparameter *die-colors* '((63 203 63) (203 203 63) (63 203 203) (203 63 203)))
(makunbound '*max-dice*)
(defparameter *max-dice* 5)
(makunbound '*ai-level*)
(defparameter *ai-level* 2)

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
                  (> (dice src) 1)
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
                      (game-tree
                        (board-attack-fail board cur-player src dst (dice src))
                        cur-player
                        (+ spare-dice (dice dst))
                        nil
                      ) ; game-tree END
                    ) ; list END
                  ) ; make-lazy END
                ) ; make-lazy END
                (lazy-nil) ; else
              ) ; if END
            ) ; lambda END
            (make-lazy (neighbors src))
          ) ; lazy-mapcan END
          (lazy-nil) ; else
        ) ; if END
      ) ; labmda END
      (make-lazy (loop for n below *board-hexnum* collect n))
    ) ; lazy-mapcan END
  ) ; labels END
) ; defun attacking-moves END

(defun board-attack-fail (board player src dst dice)
  (declare (ignore dst dice))
  (board-array
    (loop
      for pos from 0
      for hex across board
      collect
        (if (eql pos src)
          (list player 1)
          hex ; else
        ) ; if END
    ) ; loop END
  ) ; board-array END
) ; defun board-attack-fail END

;; Message is about rolling on a stack of dice with specified size:
;; "On 2 dice" means "On a stack of 2 dice", "On 5 dice" - "On a stack of 5 dice"
(defun roll-dice (dice-num)
  (let
    ( ; variable definition START
      (total
        (loop
          repeat dice-num
          sum (1+ (random 6))
        ) ; loop END
      ) ; total END
    ) ; variable definition END
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total
  ) ; let END
) ; defun roll-dice END

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice))
) ; defun roll-against END

(defun pick-chance-branch (board move)
  (labels
    ( ; function definition START
      (dice (pos) (cadr (aref board pos)))
    ) ; function definition END
    (let
      ( ; variable definition START
        (path (car move))
      ) ; variable definition END
      (if
        (or
          (null path)
          (roll-against (dice (car path)) (dice (cadr path)))
        ) ; or END
        (cadr move)
        (caddr move) ; else
      ) ; if END
    ) ; let END
  ) ; labels END
) ; defun pick-chance-branch END

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
                (princ "end turn") ; else
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
    (pick-chance-branch (cadr tree) (lazy-nth (1- (read)) moves))
  ) ; let END
) ; defun handle-human END

(fmakunbound 'web-handle-human)
(defun web-handle-human (pos)
  (cond
    ( ; 1st condition START
      (not pos)
      (princ "Please choose a hex to move from:")
    ) ; 1st condition END
    ( ; 2nd condition START
      (eq pos 'pass)
      (setf
        *cur-game-tree*
        (cadr (lazy-car (caddr *cur-game-tree*)))
      ) ; setf END
      (princ "Your reinforcements have been placed. ")
      (tag
        a ; name
        (href (make-game-link nil)) ; list of attributes
        (princ "continue") ; body
      ) ; tag END
    ) ; 2nd condition END
    ( ; 3rd condition START
      (not *from-tile*)
      (setf *from-tile* pos)
      (princ "Now choose a destination:")
    ) ; 3rd condition END
    ( ; 4th condition START
      (eq pos *from-tile*)
      (setf *from-tile* nil)
      (princ "Move cancelled.")
    ) ; 4th condition END
    ( ; default condition START
      t
      (setf
        *cur-game-tree*
        (pick-chance-branch
          (cadr *cur-game-tree*) ; board
          (lazy-find-if ; move
            (lambda (move)
              (equal (car move) (list *from-tile* pos))
            ) ; lambda END
            (caddr *cur-game-tree*)
          ) ; lazy-find-if END
        ) ; pick-chance-branch END
      ) ; setf END
      (setf *from-tile* nil)
      (princ "You may now ")
      (tag
        a ; name
        (href (make-game-link 'pass)) ; list of attributes
        (princ "pass") ; body
      ) ; tag END
      (princ " or make another move:")
    ) ; default condition END
  ) ; cond END
) ; defun web-handle-human END

(fmakunbound 'handle-computer)
(defun handle-computer (tree)
  (let
    ( ; variable definition START
      (ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree)))
    ) ; variable definition END
    (pick-chance-branch
      (cadr tree) ; board
      (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)) ; move
    ) ; pick-chance-branch END
  ) ; let END
) ; defun handle-computer END

(defparameter *dice-odds*
  #( ; anonymous array START
    #(0.84 0.97 1.0 1.0)
    #(0.44 0.78 0.94 0.99)
    #(0.15 0.45 0.74 0.91)
    #(0.04 0.19 0.46 0.72)
    #(0.01 0.06 0.22 0.46)
  ) ; anonymous array END
) ; defparameter *dice-odds* END

(fmakunbound 'get-ratings)
(defun get-ratings (tree player)
  (let
    ( ; variable definition START
      (board (cadr tree))
    ) ; variable definition END
    (labels
      ( ; function definition START
        (dice (pos) (cadr (aref board pos)))
      ) ; function definition END
      (take-all
        (lazy-mapcar
          (lambda (move)
            (let
              ( ; variable definition START
                (path (car move))
              ) ; variable definition END
              (if path
                (let*
                  ( ; variable definitions START
                    (src (car path))
                    (dst (cadr path))
                    (odds
                      (aref
                        (aref *dice-odds* (1- (dice dst)))
                        (- (dice src) 2)
                      ) ; aref END
                    ) ; odds END
                  ) ; variable definitions END
                  (+
                    (* odds (rate-position (cadr move) player))
                    (* (- 1 odds) (rate-position (caddr move) player))
                  ) ; + END
                ) ; let* END
                (rate-position (cadr move) player) ; else
              ) ; if END
            ) ; let END
          ) ; lambda END
          (caddr tree)
        ) ; lazy-mapcar END
      ) ; take-all END
    ) ; labels END
  ) ; let END
) ; defun get-ratings END

(fmakunbound 'limit-tree-depth)
(defun limit-tree-depth (tree depth)
  (list
    (car tree) ; player
    (cadr tree) ; board
    (if (zerop depth)
      (lazy-nil)
      (lazy-mapcar ; else
        (lambda (move)
          (cons
            (car move)
            (mapcar
              (lambda (x) (limit-tree-depth x (1- depth)))
              (cdr move)
            ) ; mapcar END
          ) ; cons END
        ) ; lambda END
        (caddr tree) ; '(((1stMove)(tree)) ((2ndMove)(tree)) ...)
      ) ; lazy-mapcar END
    ) ; if END
  ) ; list END
) ; defun limit-tree-depth END

(defun get-connected (board player pos)
  (labels
    ( ; function definitions START
      (check-pos (pos visited)
        (if
          (and
            (eql (car (aref board pos)) player)
            (not (member pos visited))
          ) ; and END
          (check-neighbors (neighbors pos) (cons pos visited))
          visited ; else
        ) ; if END
      ) ; check-pos END
      (check-neighbors (lst visited)
        (if lst
          (check-neighbors (cdr lst) (check-pos (car lst) visited))
          visited ; else
        ) ; if END
      ) ; check-neighbors END
    ) ; function definitions END
    (check-pos pos nil)
  ) ; labels END
) ; defun get-connected END

(defun largest-cluster-size (board player)
  (labels
    ( ; function definition START
      (f (pos visited best)
        (if (< pos *board-hexnum*)
          (if
            (and
              (eql (car (aref board pos)) player)
              (not (member pos visited))
            ) ; and END
            (let*
              ( ; variable definitions START
                (cluster (get-connected board player pos))
                (size (length cluster))
              ) ; variable definitions END
              (if (> size best)
                (f (1+ pos) (append cluster visited) size)
                (f (1+ pos) (append cluster visited) best) ; else
              ) ; if END
            ) ; let* END
            (f (1+ pos) visited best) ; else
          ) ; if END
          best ; else
        ) ; if END
      ) ; f END
    ) ; function definition END
    (f 0 nil 0)
  ) ; labels END
) ; defun largest-cluster-size END

(fmakunbound 'add-new-dice)
(defun add-new-dice (board player spare-dice)
  (declare (ignore spare-dice))
  (labels
    ( ; function definition START
      (f (lst n)
        (cond
          ( ; 1st condition START
            (zerop n)
            lst
          ) ; 1st condition END
          ( ; 2nd condition START
            (null lst)
            nil
          ) ; 2nd condition END
          ( ; default condition START
            t
            (let
              ( ; variable definitions START
                (cur-player (caar lst))
                (cur-dice (cadar lst))
              ) ; variable definitions END
              (if
                (and
                  (eql cur-player player)
                  (< cur-dice *max-dice*)
                ) ; and END
                (cons
                  (list cur-player (1+ cur-dice))
                  (f (cdr lst) (1- n))
                ) ; cons END
                (cons (car lst) (f (cdr lst) n)) ; else
              ) ; if END
            ) ; let END
          ) ; default condition END
        ) ; cond END
      ) ; f END
    ) ; function definition END
    (board-array
      (f (coerce board 'list) (largest-cluster-size board player))
    ) ; board-array END
  ) ; labels END
) ; defun add-new-dice END

; (serve #'dod-request-handler)
; http://localhost:8080/game.html

