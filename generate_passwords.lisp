; #!/usr/bin/sbcl --script

#|
Intellectual property information START

Copyright (c) 2023 Ivan Bityutskiy

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Intellectual property information END

Description START

The script produces 27 unique passwords with user defined length (1..=85).
Modify *pass-lines* variable to produce different amount of passwords.
Modify *char-array* to use different number of characters.

Usage:
clisp generate_passwords.lisp
sbcl --script generate_passwords.lisp
ccl64 -l generate_passwords.lisp -e '(quit)'

Description END
|#

;; Amount of passwords to produce
(defparameter *pass-lines* 27)

;; Array with all symbols allowed in passwords
(defparameter *char-array*
  #(
    #\# #\$ #\% #\& #\( #\) #\* #\+ #\- #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6
    #\7 #\8 #\9 #\: #\; #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H
    #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y
    #\Z #\[ #\] #\^ #\_ #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
    #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\| #\}
  ) ; #() array END 
) ; defparameter *char-array* END

(defparameter *indices* (length *char-array*))

(format t "~%Enter password's length (1..=~d): " *indices*)
(finish-output)
(defparameter *pass-length*
  (parse-integer (read-line) :junk-allowed t)
) ; defparameter *pass-length* END
(if
  (or
    (null *pass-length*)
    (< *pass-length* 1)
    (> *pass-length* *indices*)
  ) ; or END
  (setf *pass-length* *indices*)
) ; if END

;; clear screen (Unix-like systems) START
(defun clear ()
 #+clisp
  (ext:run-program "/usr/bin/clear")
 #+sbcl
  (sb-ext:run-program "/usr/bin/clear" nil :input nil :output *standard-output*)
 #+clozure
  (ccl:run-program "/usr/bin/clear" nil :input nil :output *standard-output*)
 '(screen was cleared)
) ; defun clear END
(defun clear0 () (clear))
(defun cls () (clear))
(compile 'clear)
(compile 'clear0)
(compile 'cls)
;; modern terminal emulators only
(defparameter clear (format nil "~c[3J~c[1;1H~c[0J" #\Esc #\Esc #\Esc))
(defparameter clear0 clear)
(defparameter cls clear)
;; clear screen (Unix-like systems) END

;; Create an array of unsigned 8-bit integers with limited length;
;; the length is enough for single use of the script, repeated use in
;; REPL will exhaust the array
(defun true-random-array (&optional (bit-amt 8))
  (with-open-file
    ( ; parameter definitions START
      urandom "/dev/urandom"
      :direction :input
      :element-type (list 'unsigned-byte bit-amt)
    ) ; parameter definitions END
    (let*
      ( ; variable definition START
        (uint-arr
          (make-array
            (* 10 *pass-lines* *pass-length*)
            :element-type (list 'unsigned-byte bit-amt))
        ) ; uint-arr END
      ) ; variable definition END
      (read-sequence uint-arr urandom)
      uint-arr
    ) ; let* END
  ) ; with-open-file END
) ; defun true-random-array END
(compile 'true-random-array)

(defparameter *uint-arr* (true-random-array))

;; closure 'add-character' adds random character to a list
(let
  ( ; variable definition START
    (arr-index -1)
  ) ; variable definition END
  (defun add-character (lst)
    (cons
      (aref
        *char-array*
        (mod
          (aref 
            *uint-arr*
            (incf arr-index)
          ) ; aref END
          *indices*
        ) ; mod END
      ) ; aref END
      lst
    ) ; cons END
  ) ; defun add-character END
) ; let END
(compile 'add-character)

(defun get-password ()
  (labels
    ( ; function definition START
      (f (lst)
        (if (< (length lst) *pass-length*)
          (f (remove-duplicates (add-character lst)))
          (format t "~a~&" (coerce lst 'string))
        ) ; if END
      ) ; f END
    ) ; function definition END
    (f nil)
  ) ; labels END
) ; defun get-password END
(compile 'get-password)

(defun get-passwords ()
  (terpri)
  (loop
    repeat *pass-lines*
    do
      (get-password)
  ) ; loop END
  (terpri)
) ; defun get-password END
(compile 'get-passwords)

(get-passwords)

;; END OF SCRIPT
