;;;; CHAPTER 11 (Land Of Lisp)

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

;; Play the classic game of Robots! All robots move towards the player.
;; Robot collisions cause scrap that is deadly to other robots.
;; Teleport as a last resort!

(defun robots ()
  (loop
    ; By naming it 'main', we can use 'return-from' to exit early
    named main
    ; These are the 8 offsets when the game board is 64 wide
    with directions = '((q . -65) (w . -64) (e . -63) (a . -1) (d . 1) (z . 63) (x . 64) (c . 65))
    for pos = 544
    then
      (progn
        (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave: ")
        (force-output)
        (let*
          ( ; variable definitions START
            (c (read))
            ; 'assoc' performs a lookup
            (d (assoc c directions))
          ) ; variable definitions END
          (cond
            ( ; 1st condition START
              d
              (+ pos (cdr d))
            ) ; 1st condition END
            ( ; 2nd condition START
              (eq 't c)
              ; The game board is 64 x 16 = 1024; (random 1024) picks a random spot
              (random 1024)
            ) ; 2nd condition END
            ( ; 3rd condition START
              (eq 'l c)
              ; Player wants to leave the game
              (return-from main 'bye)
            ) ; 3rd condition END
            ( ; default condition START
              t
              pos
            ) ; default condition END
          ) ; cond END
        ) ; let* END
      ) ; progn END
    ; Change 'repeat 10' for the amount of robots
    for monsters = (loop repeat 10 collect (random 1024))
    then
      (loop
        for mpos in monsters
        collect
          (if
            (> (count mpos monsters) 1)
            mpos
            (cdar
              ; We sort locations based on distance to player, then chomp of the closest
              (sort
                (loop
                  for (k . d) in directions
                  for new-mpos = (+ mpos d)
                  collect
                    (cons
                      ; This calculates the 'Manhattan distance' to the player
                      (+
                        (abs (- (mod new-mpos 64) (mod pos 64)))
                        (abs (- (ash new-mpos -6) (ash pos -6)))
                      ) ; + END
                      new-mpos
                    ) ; cons END
                ) ; loop END
                '<
                :key #'car
              ) ; sort END
            ) ; cdar END
          ) ; if END
      ) ; loop END
    when
      ; Checks if all monsters are scrap
      ; More than 1 robot in a spot means they're scrap
      (loop for mpos in monsters always (> (count mpos monsters) 1))
      do (clear)
        and
          return 'player-wins
    do (clear)
    do
      (format
        t
        ; '~{ ~}' loops through list, applies control string to each item in the list
        ; '~:;' causes text to be broken into pieces of equal length
        ; When inside justification '~< ~>',
        ; control string '|~%|' preceding '~,65:;' is triggered if cursor position is beyond 65
        ; '|~%|' is 'vertical bar', 'new-line', 'vertical bar'
        ; '~a' prints output similar to 'princ' function
        "~%|~{~<|~%|~,65:;~a~>~}|"
        (loop
          ; loop through board positions
          for p below 1024
          collect
            (cond
              ( ; 1st condition START
                (member p monsters)
                (cond
                  ( ; 1st condition START
                    (= p pos)
                    ; If player is in the same spot as a robot, player loses
                    (return-from main 'player-loses)
                  ) ; 1st condition END
                  ( ; 2nd condition START
                    ; Draw scrap for doubled-up robots
                    (> (count p monsters) 1)
                    #\#
                  ) ; 2nd condition END
                  ( ; default condition START
                    t
                    #\A
                  ) ; default condition END
                ) ; cond END
              ) ; 1st condition END
              ( ; 2nd condition START
                (= p pos)
                #\@
              ) ; 2nd condition END
              ( ; default condition START
                t
                #\Space
              ) ; default condition END
            ) ; cond END
        ) ; loop END
      ) ; format END
  ) ; loop END
) ; defun robots END

