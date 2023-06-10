;;;; CHAPTER 19 (Land Of Lisp)

;; clear screen START
;; This 'clear' function will work only on modern terminal emulators
; (defun clear ()
;   (format nil "~c[3J~c[1;1H~c[0J" #\Esc #\Esc #\Esc)
; ) ; defun clear END
; (defun clear0 () (clear))
; (defun cls () (clear))
;; In this case variable 'clear' can be used instead of a function 'clear'
;; Typing 'clear' in REPL return the terminal contol sequence to clear screen
; (defparameter clear
;   (coerce '(#\Esc #\[ #\3 #\J #\Esc #\[ #\1 #\; #\1 #\H #\Esc #\[ #\0 #\J) 'string)
; ) ; defparameter clear END
; (defparameter clear (format nil "~c[3J~c[1;1H~c[0J" #\Esc #\Esc #\Esc))
; (defparameter clear0 clear)
; (defparameter cls clear)
; (format t "~c[3J~c[1;1H~c[0J" #\Esc #\Esc #\Esc)
;; clear screen END

;; Script works in Clisp, Sbcl and Clozure common lisp implementations.
;; Sbcl is fastest and provide the best gaming experience for a player.
;; ccl64 -l ~/lisp/lol/ch19/dice_of_doom_v3.lisp -e "(serve #'dod-request-handler)"

(load "dice_of_doom_v2.lisp")
(load "webserver.lisp")
(load "svg.lisp")

;;;; Graphical side of Dice of Doom version 3
(defparameter *board-width* 900)
(defparameter *board-height* 500)
(defparameter *board-scale* 64)
(defparameter *top-offset* 3)
(defparameter *dice-scale* 40)
(defparameter *dot-size* 0.05)

(defun draw-die-svg (x y col)
  (labels
    ( ; function definitions START
      (calc-pt (pt)
        (cons
          (+ x (* *dice-scale* (car pt)))
          (+ y (* *dice-scale* (cdr pt)))
        ) ; cons END
      ) ; calc-pt END
      (f
        (pol col)
        (polygon (mapcar #'calc-pt pol) col)
      ) ; f END
    ) ; function definitions END
    (f
      '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
      (brightness col 40)
    ) ; f END
    (f
      '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
      col
    ) ; f END
    (f
      '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
      (brightness col -40)
    ) ; f END
    (mapc
      (lambda (x y)
        (polygon
          (mapcar ; points
            (lambda (xx yy)
              (calc-pt
                (cons
                  (+ x (* xx *dot-size*))
                  (+ y (* yy *dot-size*))
                ) ; cons END
              ) ; calc-pt END
            ) ; lambda END
            '(-1 -1 1 1)
            '(-1 1 1 -1)
          ) ; mapcar END
          '(255 255 255) ; color
        ) ; polygon END
      ) ; lambda END
      '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
      '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625 -0.35 -0.05 -0.45 -0.15 -0.45 -0.05)
    ) ; mapc END
  ) ; labels END
) ; defun draw-die-svg END

; (svg 100 100 (draw-die-svg 50 50 '(255 0 0)))

(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  (loop
    for z below 2
    do
      (polygon
        (mapcar ; points
          (lambda (pt)
            (cons
              (+ xx (* *board-scale* (car pt)))
              (+
                yy
                (*
                  *board-scale*
                  (+ (cdr pt) (* (- 1 z) 0.1))
                ) ; * END
              ) ; + END
            ) ; cons END
          ) ; lambda END
          '((-1 . -0.2) (0 . -0.5) (1 . -0.2)
          (1 . 0.2) (0 . 0.5) (-1 . 0.2))
        ) ; mapcar END
        (if (eql pos chosen-tile) ; color
          (brightness col 100)
          col
        ) ; if END
      ) ; polygon END
  ) ; loop END
  (loop
    for z below (second hex)
    do
      (draw-die-svg
        (+ ; x
          xx
          (*
            *dice-scale*
            0.3
            (if (oddp (+ x y z))
              -0.3
              0.3
            ) ; if END
          ) ; * END
        ) ; + END
        (- yy (* *dice-scale* z 0.8)) ; y
        col ; color
      ) ; draw-die-svg END
  ) ; loop END
) ; defun draw-tile-svg END

; (svg 300 300 (draw-tile-svg 0 0 0 '(0 3) 100 150 '(255 0 0) nil))

(defparameter *die-colors* '((63 203 63) (203 203 63)))

(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop
    for y below *board-size*
    do
      (loop
        for x below *board-size*
        for pos = (+ x (* *board-size* y))
        for hex = (aref board pos)
        for xx = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
        for yy = (* *board-scale* (+ (* y 0.7) *top-offset*))
        for col = (brightness (nth (car hex) *die-colors*) (* -15 (- *board-size* y)))
        do
          (if (member pos legal-tiles)
            (tag
              g ; name
              nil ; list of attributes
              (tag ; body containing another 'tag'
                a ; name
                ("xlink:href" (make-game-link pos)) ; list of attributes
                (draw-tile-svg x y pos hex xx yy col chosen-tile) ; body
              ) ; tag END
            ) ; tag END
            (draw-tile-svg x y pos hex xx yy col chosen-tile) ; else
          ) ; if END
      ) ; loop END
  ) ; loop END
) ; defun draw-board-svg END

(defun make-game-link (pos)
  (format nil "/game.html?chosen=~a" pos)
) ; defun make-game-link END

; (svg *board-width* *board-height* (draw-board-svg (gen-board) nil nil))

;;;; Server side of Dice of Doom version 3

;; Web request handler
(defparameter *cur-game-tree* nil)
(defparameter *from-tile* nil)

;; 'path' is a string: "game.html"
;; 'header' is an alist: '((ACCEPT-LANGUAGE . "en-US,en;q=0.5") (ACCEPT-ENCODING . "gzip, deflate, br"))
;; 'header' is sent by client browser, this request handler is not using it; a header from server
;; is sent by using 'princ' and using appropriate line termination characters #\Return and #\Linefeed
;; 'params' is an alist: '((NAME . "Bob") (AGE . "25") (GENDER . "male"))
(defun dod-request-handler (path header params)
  (if (equal path "game.html")
    (progn
      (princ "HTTP/1.1 200 OK") (princ #\Return) (princ #\Linefeed)
      (princ "Cache-Control: no-store") (princ #\Return) (princ #\Linefeed)
      (princ "Content-Type: text/html; charset=utf-8") (princ #\Return) (princ #\Linefeed)
      (princ #\Return) (princ #\Linefeed) ; END of server's header message
      (princ "<!DOCTYPE html>")
      (html
        (tag
          head ; name
          nil ; list of attributes
          (tag
            title ; name
            nil ; list of attributes
            (princ "DICE OF DOOM :: ")
            #+clisp (princ "CLISP") ; GNU CLISP
            #+sbcl (princ "SBCL") ; Steel Bank Common Lisp
            #+clozure (princ "CCL") ; Clozure Common Lisp
          ) ; tag END
        ) ; tag END
        (body
          (tag
            center ; name
            nil ; list of attributes
            (tag
              h2 ; name
              nil ; list of attributes
              (princ "Welcome to DICE OF DOOM!")
            ) ; tag END
          ) ; tag END
          (tag
            p ; name
            nil ; list of attributes
            (tag
              a ; name
              (href "game.html") ; list of attributes
              (princ "NEW GAME") ; body of 'tag'
            ) ; tag END
          ) ; tag END
          (tag
            center ; name
            nil ; list of attributes
            (let
              ( ; variable definition START
                (chosen (assoc 'chosen params))
              ) ; variable definition END
              (when (or (not *cur-game-tree*) (not chosen))
                (setf chosen nil)
                (web-initialize)
              ) ; when END
              (cond
                ( ; 1st condition START
                  (lazy-null (caddr *cur-game-tree*))
                  (web-announce-winner (cadr *cur-game-tree*))
                ) ; 1st condition END
                ( ; 2nd condition START
                  (zerop (car *cur-game-tree*))
                  (web-handle-human
                    (when chosen
                      (read-from-string (cdr chosen))
                    ) ; when END
                  ) ; web-handle-human END
                ) ; 2nd condition END
                ( ; default condition START
                  t
                  (web-handle-computer)
                ) ; default condition END
              ) ; cond END
            ) ; let END
            (princ "<br />")
            (draw-dod-page *cur-game-tree* *from-tile*)
          ) ; tag END
          (tag
            p ; name
            nil ; list of attributes
            (tag
              a ; name
              (href "shutdown.html") ; list of attributes
              (princ "SHUTDOWN SERVER") ; body of 'tag'
            ) ; tag END
          ) ; tag END
        ) ; body END
      ) ; html END
      (princ #\Return) (princ #\Linefeed)
    ) ; progn END
    (if (string-equal path "shutdown.html")
      'shutdown
      (progn
        (princ "HTTP/1.1 404 Not Found") (princ #\Return) (princ #\Linefeed)
        (princ "Cache-Control: no-store") (princ #\Return) (princ #\Linefeed)
        (princ "Content-Type: text/html; charset=utf-8") (princ #\Return) (princ #\Linefeed)
        (princ #\Return) (princ #\Linefeed) ; END of server's header message
        (princ "<!DOCTYPE html>")
        (html
          (tag
            head ; name
            nil ; list of attributes
            (tag
              title ; name
              nil ; list of attributes
              (princ "Something went wrong! :: ")
              #+clisp (princ "CLISP") ; GNU CLISP
              #+sbcl (princ "SBCL") ; Steel Bank Common Lisp
              #+clozure (princ "CCL") ; Clozure Common Lisp
            ) ; tag END
          ) ; tag END
          (body
            (tag
              center ; name
              nil ; list of attributes
              (tag
                h2 ; name
                nil ; list of attributes
                (princ "Sorry... I don't know that page.")
              ) ; tag END
              (tag
                p ; name
                nil ; list of attributes
                (tag
                  a ; name
                  (href "game.html") ; list of attributes
                  (princ "Play DICE OF DOOM!") ; body of 'tag'
                ) ; tag END
              ) ; tag END
            ) ; tag END
            (tag
              p ; name
              nil ; list of attributes
              (tag
                a ; name
                (href "shutdown.html") ; list of attributes
                (princ "SHUTDOWN SERVER") ; body of 'tag'
              ) ; tag END
            ) ; tag END
          ) ; body END
        ) ; html END
        (princ #\Return) (princ #\Linefeed)
      ) ; progn END
    ) ; if END
  ) ; if END
) ; defun dod-request-handler END

(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t))
) ; defun web-initialize END

(defun player-name (n)
  (if (zerop n)
    "<strong>HUMAN</strong>"
    "<strong>COMPUTER</strong>"
  ) ; if END
) ; defun player-name END

(defun web-announce-winner (board)
  (let
    ( ; variable definition START
      (w (winners board))
    ) ; variable definition END
    (if (> (length w) 1)
      (format t "The game is a tie between ~a " (mapcar #'player-name w))
      (format t "The winner is ~a " (player-name (car w)))
    ) ; if END
  ) ; let END
  (tag
    a ; name
    (href "game.html") ; list of attributes
    (princ "play again") ; body
  ) ; tag END
) ; defun web-announce-winner END

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
        (cadr
          (lazy-find-if
            (lambda (move)
              (equal (car move) (list *from-tile* pos))
            ) ; lambda END
            (caddr *cur-game-tree*)
          ) ; lazy-find-if END
        ) ; cadr END
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

(defun web-handle-computer ()
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved.")
  (tag
    script ; name
    (type "text/javascript") ; list of attributes
    (princ "window.setTimeout('window.location=\"game.html?chosen=NIL\"', 5000)") ; body
  ) ; tag END
) ; defun web-handle-computer END

(defun draw-dod-page (tree selected-tile)
  (svg
    *board-width*
    *board-height*
    (draw-board-svg
      (cadr tree)
      selected-tile
      (take-all
        (if selected-tile
          (lazy-cons
            selected-tile
            (lazy-mapcar
              (lambda (move)
                (when (eql (caar move) selected-tile)
                  (cadar move)
                ) ; when END
              ) ; lambda END
              (caddr tree)
            ) ; lazy-mapcar END
          ) ; lazy-cons END
          (lazy-mapcar #'caar (caddr tree)) ; else
        ) ; if END
      ) ; take-all END
    ) ; draw-board-svg END
  ) ; svg END
) ; defun draw-dod-page END

; (serve #'dod-request-handler)
; http://localhost:8080/game.html

