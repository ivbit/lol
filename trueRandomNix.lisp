;;;; TRUE RANDOM NUMBER GENERATOR IN COMMON LISP for Unix-like systems

;; clear screen START
(defun clear ()
  #+clisp
    (ext:run-program "/usr/bin/clear")
  #+sbcl
    (sb-ext:run-program "/usr/bin/clear" '() :input nil :output *standard-output*)
  #+clozure
    (ccl:run-program "/usr/bin/clear" '() :input nil :output *standard-output*)
  '(screen was cleared)
) ; defun clear END
(defun clear0 () (clear))
(defun cls () (clear))
;; clear screen END

;; 'true-random' returns a true random number, 'bit-amt' can be set to higher
;; or lower values to affect the size of a true random number (true-random 128)
;; allowed values are powers of 2 starting from 8:
;; 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8096, ..., 65536, ...
;; (loop for i from 3 upto 20 collect (expt 2 i))
;; 64 is the limit for 'Clozure common lisp' implementation
;; 1024 is the limit for 'Sbcl' implementation
(defun true-random (&optional (bit-amt 64))
  (with-open-file
    ( ; parameter definitions START
      urandom "/dev/urandom"
      :direction :input
      :element-type (list 'unsigned-byte bit-amt)
    ) ; parameter definitions END
    (let*
      ( ; variable definition START
        (temp-array (make-array 1 :element-type (list 'unsigned-byte bit-amt)))
      ) ; variable definition END
      (read-sequence temp-array urandom)
      (aref temp-array 0)
    ) ; let* END
  ) ; with-open-file END
) ; defun get-random-uint END

;; get true random number from 1 upto 200
(1+ (mod (true-random 8) 200))

;; get true random number from 0 upto 4500
(mod (true-random 16) 4501)

;; get 100 true random numbers from 0 upto 5195173
;; Clisp and Sbcl only. For Clozure change to (true-random)
;; (true-random) is the same as (true-random 64) - 64 is the default value
;; (true-random 32) would be enough for 5195174, however
;; (true-random 128) will produce better true random numbers
(loop
  for i below 100
  collect (mod (true-random 128) 5195174)
) ; loop END

;;;; (true-random 64) is a limit for 'Clozure common lisp':
;;;; 128 and higher will produce an error:
;;;; 'Error: Not an ivector subtag: 182
;;;; While executing: CCL::SUBTAG-BYTES, in process listener(1).'

