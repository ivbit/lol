

;; Players generally sit in a circle. The player designated to go first
;; says the number "1", and the players then count upwards in turn.
;; However, any number divisible by "3" is replaced by the word Fizz and
;; any number divisible by "5" by the word Buzz.
;; Numbers divisible by both "3" and "5" (i.e. divisible by "15") become
;; Fizz Buzz. A player who hesitates or makes a mistake is eliminated. 

(terpri)
(dotimes (i 101)
  (cond 
    ( ; 1st condition START
      (zerop i)
      (princ "\"Fizz Buzz\" game:")
      (fresh-line)
    ) ; 1st condition END
    ( ; 2nd condition START
      (and
        (zerop (mod i 3))
        (zerop (mod i 5))
      ) ; and END
      (princ "Fizz Buzz")
      (fresh-line)
    ) ; 2nd condition END
    ( ; 3rd condition START
      (zerop (mod i 3))
      (princ "Fizz")
      (fresh-line)
    ) ; 3rd condition END
    ( ; 4th condition START
      (zerop (mod i 5))
      (princ "Buzz")
      (fresh-line)
    ) ; 4th condition END
    ( ; default condition START
      t
      (princ i)
      (fresh-line)
    ) ; default condition END
  ) ; cond END
) ; dotimes END
(terpri)


