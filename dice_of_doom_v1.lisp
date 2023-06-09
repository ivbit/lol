;;;; CHAPTER 15 (Land Of Lisp)

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
;; modern terminal emulators only
(defparameter clear (format nil "~c[3J~c[1;1H~c[0J" #\Esc #\Esc #\Esc))
(defparameter clear0 clear)
(defparameter cls clear)
;; clear screen END

;;;; Dice of doom

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; functional
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst)
) ; defun board-array END

;; imperative
(defun gen-board ()
  (board-array
    (loop
      for n below *board-hexnum*
      collect
        (list
          (random *num-players*)
          (1+ (random *max-dice*))
        ) ; list END
    ) ; loop END
  ) ; board-array END
) ; defun gen-board END

;; functional
(defun player-letter (n)
  (code-char (+ 97 n))
) ; defun player-letter END

;; imperative
(defun draw-board (board)
  (loop for y below *board-size*
    do
      (progn
        (fresh-line)
        (loop
          repeat (- *board-size* y)
          do (princ "  ")
        ) ; loop END
        (loop
          for x below *board-size*
          for hex = (aref board (+ x (* *board-size* y)))
          do (format t "~a-~a " (player-letter (first hex)) (second hex))
        ) ; loop END
      ) ; progn END
  ) ; loop END
) ; defun draw-board END

; (draw-board #((0 3) (0 3) (1 3) (1 1)))

;; functional
(defun game-tree (board player spare-dice first-move)
  (list
    player
    board
    (add-passing-move
      board
      player
      spare-dice
      first-move
      (attacking-moves board player spare-dice)
    ) ; add-passing-move END
  ) ; list END
) ; defun game-tree END

;; functional
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (cons
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
    ) ; cons END
  ) ; if END
) ; defun add-passing-move END

;; functional
(defun attacking-moves (board cur-player spare-dice)
  (labels
    ( ; function definitions START
      (player (pos) (car (aref board pos)))
      (dice (pos) (cadr (aref board pos)))
    ) ; function definitions END
    (mapcan
      (lambda (src)
        (when (eql (player src) cur-player)
          (mapcan
            (lambda (dst)
              (when
                (and
                  (not (eql (player dst) cur-player))
                  (> (dice src) (dice dst))
                ) ; and END
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
              ) ; when END
            ) ; lambda END
            (neighbors src)
          ) ; mapcan END
        ) ; when END
      ) ; lambda END
      (loop for n below *board-hexnum* collect n)
    ) ; mapcan END
  ) ; labels END
) ; defun attacking-moves END

;; functional
(defun neighbors (pos)
  (let
    ( ; variable definitions START
      (up (- pos *board-size*))
      (down (+ pos *board-size*))
    ) ; variable definitions END
    (loop
      for p in
        (append
          (list up down)
          (unless (zerop (mod pos *board-size*))
            (list (1- up) (1- pos))
          ) ; unless END
          (unless (zerop (mod (1+ pos) *board-size*))
            (list (1+ pos) (1+ down))
          ) ; unless END
        ) ; append END
      when (and (>= p 0) (< p *board-hexnum*))
        collect p
    ) ; loop END
  ) ; let END
) ; defun neighbors END

;; functional
(defun board-attack (board player src dst dice)
  (board-array
    (loop
      for pos from 0
      for hex across board
      collect
        (cond
          ( ; 1st condition START
            (eql pos src)
            (list player 1)
          ) ; 1st condition END
          ( ; 2nd condition START
            (eql pos dst)
            (list player (1- dice))
          ) ; 2nd condition END
          ( ; default condition START
            t
            hex
          ) ; default condition END
        ) ; cond END
    ) ; loop END
  ) ; board-array END
) ; defun board-attack END

;; functional
(defun add-new-dice (board player spare-dice)
  (labels
    ( ; function definition START
      (f (lst n)
        (cond
          ( ; 1st condition START
            (null lst)
            nil
          ) ; 1st condition END
          ( ; 2nd condition START
            (zerop n)
            lst
          ) ; 2nd condition END
          ( ; default condition START
            t
            (let
              ( ; variable definitions START
                (cur-player (caar lst))
                (cur-dice (cadar lst))
              ) ; variable definitions END
              (if (and (eql cur-player player) (< cur-dice *max-dice*))
                (cons
                  (list cur-player (1+ cur-dice))
                  (f (cdr lst) (1- n))
                ) ; cons END
                (cons (car lst) (f (cdr lst) n))
              ) ; if END
            ) ; let END
          ) ; default condition END
        ) ; cond END
      ) ; f END
    ) ; function definition END
    (board-array (f (coerce board 'list) spare-dice))
  ) ; labels END
) ; defun add-new-dice END

;; imperative
(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))
  ) ; if END
) ; defun play-vs-human END

;; imperative
(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree))
) ; defun print-info END

;; imperative
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let
    ( ; variable definition START
      (moves (caddr tree))
    ) ; variable definition END
    (loop
      for move in moves
      for n from 1
      do
        (let
          ( ; variable definition START
            (action (car move))
          ) ; variable definition END
          (fresh-line)
          (format t "~a. " n)
          (if action
            (format t "~a -> ~a" (car action) (cadr action))
            (princ "end turn")
          ) ; if END
        ) ; let END
    ) ; loop END
    (fresh-line)
    (finish-output)
    (cadr ; returns a game tree
      (nth
        (1- (read))
        moves ; (caddr tree)
      ) ; nth END
    ) ; cadr END
  ) ; let END
) ; defun handle-human END

;; functional
(defun winners (board)
  (let*
    ( ; variable definitions START
      (tally
        (loop
          for hex across board
          collect (car hex)
        ) ; loop END
      ) ; tally END
      (totals
        (mapcar
          (lambda (player)
            (cons player (count player tally))
          ) ; lambda END
          (remove-duplicates tally)
        ) ; mapcar END
      ) ; totals END
      (best (apply #'max (mapcar #'cdr totals)))
    ) ; variable definitions END
    (mapcar
      #'car
      (remove-if
        (lambda (x)
          (not (eql (cdr x) best))
        ) ; lambda END
        totals
      ) ; remove-if END
    ) ; mapcar END
  ) ; let* END
) ; defun winners END

;; imperative
(defun announce-winner (board)
  (fresh-line)
  (let
    ( ; variable definition START
      (w (winners board))
    ) ; variable definition END
    (if (> (length w) 1)
      (format t "The game is a tie between ~a" (mapcar #'player-letter w))
      (format t "The winner is ~a" (player-letter (car w)))
    ) ; if END
  ) ; let END
) ; defun announce-winner END

; (play-vs-human (game-tree (gen-board) 0 0 t))

;; functional
(defun rate-position (tree player)
  (let
    ( ; variable definition START
      (moves (caddr tree))
    ) ; variable definition END
    (if moves
      (apply
        (if (eql (car tree) player)
          #'max
          #'min
        ) ; if END
        (get-ratings tree player)
      ) ; apply END
      (let
        ( ; variable definition START
          (w (winners (cadr tree)))
        ) ; variable definition END
        (if (member player w)
          (/ 1 (length w))
          0
        ) ; if END
      ) ; let END
    ) ; if END
  ) ; let END
) ; defun rate-position END

;; functional
(defun get-ratings (tree player)
  (mapcar
    (lambda (move)
      (rate-position (cadr move) player)
    ) ; lambda END
    (caddr tree)
  ) ; mapcar END
) ; defun get-ratings END

;; imperative
(defun handle-computer (tree)
  (let
    ( ; variable definition START
      (ratings (get-ratings tree (car tree)))
    ) ; variable definition END
    (cadr ; returns a game tree
      (nth
        (position
          (apply #'max ratings)
          ratings
        ) ; position END
        (caddr tree)
      ) ; nth END
    ) ; cadr END
  ) ; let END
) ; defun handle-computer END

;; imperative
(defun play-vs-computer (tree)
  (print-info tree)
  (cond
    ( ; 1st condition START
      (null (caddr tree))
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

; (play-vs-computer (game-tree (gen-board) 0 0 t))

;;;; Making the game faster
;; memoization using closures
(defparameter *board-size* 3)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(let
  ( ; lexical variable definitions START
    (old-neighbors (symbol-function 'neighbors))
    (previous (make-hash-table))
  ) ; lexical variable definitions END
  (defun neighbors (pos)
    (or
      (gethash pos previous)
      (setf (gethash pos previous) (funcall old-neighbors pos))
    ) ; or END
  ) ; defun neighbors END
) ; let END

;; &rest - refer to all arguments equally as a single aggregation
(let
  ( ; lexical variable definitions START
    (old-game-tree (symbol-function 'game-tree))
    (previous (make-hash-table :test #'equalp))
  ) ; lexical variable definitions END
  (defun game-tree (&rest rest)
    (or
      (gethash rest previous) ; rest is '(board player spare-dice first-move)
      (setf (gethash rest previous) (apply old-game-tree rest))
    ) ; or END
  ) ; defun game-tree END
) ; let END

(let
  ( ; lexical variable definitions START
    (old-rate-position (symbol-function 'rate-position))
    (previous (make-hash-table))
  ) ; lexical variable definitions END
  (defun rate-position (tree player)
    (let
      ( ; variable definition START
        (tab (gethash player previous))
      ) ; variable definition END
      (unless tab
        (setf
          tab
          ; a game tree object can not be compared with 'eql' (the book is wrong)
          (setf (gethash player previous) (make-hash-table :test #'equalp))
        ) ; setf END
      ) ; unless END
      (or
        (gethash tree tab)
        (setf
          (gethash tree tab)
          (funcall old-rate-position tree player)
        ) ; setf END
      ) ; or END
    ) ; let END
  ) ; defun rate-position END
) ; let END

;; tail-call optimization
(defun add-new-dice (board player spare-dice)
  (labels
    ( ; function definition START
      (f (lst n acc)
        (cond
          ( ; 1st condition START
            (zerop n)
            (append (reverse acc) lst)
          ) ; 1st condition END
          ( ; 2nd condition START
            (null lst)
            (reverse acc)
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
                (f
                  (cdr lst)
                  (1- n)
                  (cons (list cur-player (1+ cur-dice)) acc)
                ) ; f END
                (f
                  (cdr lst)
                  n
                  (cons (car lst) acc)
                ) ; f END
              ) ; if END
            ) ; let END
          ) ; default condition END
        ) ; cond END
      ) ; f END
    ) ; function definition END
    (board-array (f (coerce board 'list) spare-dice nil))
  ) ; labels END
) ; defun add-new-dice END

;; Enable tail-call optimization in CLISP for 'add-new-dice' function
(compile 'add-new-dice)

