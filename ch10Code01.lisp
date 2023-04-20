;;;; CHAPTER 10 (Land Of Lisp)

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

;; Add together natural numbers below 5:
;; 0 + 1 + 2 + 3 + 4 = 10
(loop
  for i
  below 5
  sum i
)

;; Counting from a starting point to an ending point
(loop
  for i
  from 5
  to 10
  sum i
)

;; Iterating through values in a list
(loop
  for i
  in '(100 20 3)
  sum i
)

;; Doing stuff in a loop
(loop
  for i
  below 5
  do (print i)
)

;; Doing stuff under certain conditions
(loop
  for i
  below 10
  when (oddp i)
  sum i
)

;; Breaking out of a loop early
(loop
  for i
  from 0
  do (print i)
  when (= i 5)
  return 'falafel
)

;; Collecting a list of values
(loop
  for i
  in '(2 3 4 5 6)
  collect (* i i)
)

;; Using multiple 'for' clauses
(loop
  for x below 10
  for y below 10
  collect (+ x y)
)

;; Cartesian product:
;; loop will run once for every possible combination of 2 ranges.
(loop
  for x below 10
  collect
    (loop
      for y below 10
      collect (+ x y)
    )
)

;; Track the index number of items in a list: i == 0
(loop
  for i
  from 0
  for day
  in '(monday tuesday wednesday thursday friday saturday sunday)
  collect (cons i day)
)

;; Simple loop
(loop
  (princ "type something: ")
  (force-output)
  (read)
)

;; do, doing
(loop
  for i
  below 5
  do (print i)
)

;; repeat
(loop
  repeat 5
  do (print "Prints five times")
)

;; return
(loop
  for i
  below 10
  when (= i 5)
  return 'leaving-early
  do (print i)
)

;; initially
(loop
  initially
    (print 'loop-begin)
  for x
  below 3
  do (print x)
)

;; finally
(loop
  for x
  below 3
  do (print x)
  finally
    (print 'loop-end)
)

;; named, return-from
(loop
  named outer
  for i below 10
  do
    (progn
      (print "outer")
      (loop
        named inner
        for x
        below i
        do (print "**inner")
        when (= x 2)
        do (return-from outer 'kicked-out-all-the-way)
      ) ; loop END
    ) ; progn END
) ; loop END

;; while
(loop
  for i
  in '(0 2 4 555 6)
  while (evenp i)
  do (print i)
)

;; until
(loop
  for i
  from 0
  do (print i)
  until (> i 3)
)

;; with - create a local variable
(loop
  with x = (+ 1 2)
  repeat 5
  do (print x)
)

;; being - hash tables
(defparameter salary (make-hash-table))
(setf (gethash 'bob salary) 80)
(setf (gethash 'john salary) 90)
(loop
  for person
  being each hash key of salary
  do (print person)
)

;; using - hash tables
(loop
  for person
  being each hash-key of salary
  using (hash-value amt)
  do (print (cons person amt))
)

;; 'the' - same as 'each'; 'hash-keys' - same as 'hash-key'
(loop
  for person
  being the hash-keys of salary
  do (print person)
)
(loop
  for person
  being each hash-key of salary
  do (print person)
)

;; 'hash-values' - same as 'hash-value'
(loop
  for amt
  being the hash-values of salary
  do (print amt)
)
(loop
  for amt
  being each hash-value of salary
  do (print amt)
)

;; 'for' - same as 'as'
(loop
  for i
  from 0
  do (print i)
  when (= i 5)
  return 'zuchini
)
(loop
  as x
  from 5
  to 10
  collect x
)

;; 'in' - NOT same as 'on'
(loop
  for i
  in '(100 20 3)
  sum i
)
(loop
  for x
  on '(1 3 5)
  do (print x)
)

;; by
(loop
  for i
  from 6
  to 8
  by 2
  sum i
)

;; then
(loop
  for x = 10.0
  then (/ x 2)
  repeat 5
  collect x
)

;; from, to
(loop
  for i
  from 6
  to 8
  sum i
)

;; upfrom
(loop
  for i
  upfrom 6
  to 8
  sum i
)

;; upto
(loop
  for i
  from 6
  upto 8
  sum i
)

;; downfrom
(loop
  for i
  downfrom 10
  to 7
  do (print i)
)

;; downto
(loop
  for i
  from 10
  downto 7
  do (print i)
)

;; 'across' - for arrays
(loop
  for i
  across #(100 20 3)
  sum i
)

;; 'into' - lets you create local variables to return
(loop
  for i
  in '(3 8 73 4 -5)
  minimize i into lowest
  maximize i into biggest
  finally
    (return (cons lowest biggest))
)

;; 'always' - check collection for truth of a condition
(loop
  for i
  in '(0 2 4 6)
  always (evenp i)
)

;; 'never' - check collection for truth of a condition
(loop
  for i
  in '(0 2 4 6)
  never (oddp i)
)

;; 'thereis' - check collection for truth of a condition
(loop
  for i
  in '(0 2 4 555 6)
  thereis (oddp i)
)

;; 'if' - building conditions
(loop
  for i
  below 5
  if (oddp i)
    do (print i)
)

;; 'when' - building conditions
(loop
  for i
  below 4
  when (oddp i)
    do (print i)
  do (print "yup")
)

;; 'unless' - building conditions
(loop
  for i
  below 4
  unless (oddp i)
    do (print i)
)

;; 'and' - building conditions
(loop
  for x
  below 5
  when (= x 3)
    do (print "do this")
    and
    do (print "also do this")
  do (print "ALWAYS DO THIS")
)

;; 'else' - building conditions
(loop
  for i
  below 5
  if (oddp i)
    do (print i)
  else
    do (print "w00t")
)

;; 'end' - building conditions
;; Withouth the 'end' preposition, the last 'and' would apply to the
;; inner 'if' rather than the outer 'if'
(loop
  for x
  from 0
  to 3
  do (print x)
  if (zerop (mod x 2))
    do (princ " a")
    and
      if (zerop (floor x 2))
        do (princ " b")
  end
  and do (princ " c")
)

;;;; Extracting a result
;; count, counting
(loop
  for i
  in '(1 1 1 1)
  count i
)

;; sum, summing
(loop
  for i
  below 5
  sum i
)

;; minimize, minimizing
(loop
  for i
  in '(3 2 1 2 3)
  minimize i
)

;; maximize, maximizing
(loop
  for i
  in '(1 2 3 2 1)
  maximize i
)

;; append, appending
(loop
  for i
  below 5
  append (list 'Z i)
)

;; nconc, nconcing
;; similar to 'append', but has side effects:
;; the lists are modified rather than copied.
(loop
  for i
  below 5
  nconc (list 'Z i)
)

