;;;; CHAPTER 5 (Land Of Lisp)

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

;; *nodes* is a structure called "association list" (alist),
;; where key = location name, data = description.
(defparameter *nodes*
  '( ; alist of *nodes* START
    ( ; node 1 START
      living-room
      (you are in the living-room. there is a wizard snoring loudly on the couch.)
    ) ; node 1 END
    ( ; node 2 START
      garden
      (you are in a beautiful garden. there is a well in front of you.)
    ) ; node 2 END
    ( ; node 3 START
      attic
      (you are in the attic. there is a giant welding torch in the corner.)
    ) ; node 3 END
  ) ; alist of *nodes* END
) ; defparameter *nodes* END

;; 1st argument is location symbol, 2nd argument is alist of *nodes*
(defun describe-location (location nodes)
  ; from the list "cadr" returns the 2nd item
  ; (cadr '(foo bar baz) ) returns BAR
  (cadr
    ; from the big list "assoc" returns first nested list which has
    ; a symbol stored in location variable as 1st item of the list
    ; (assoc 'abc '( (foo bar) (abc xyz) ) ) returns (ABC XYZ)
    (assoc location nodes)
  ) ; cadr END
) ; defun describe-location END

(defparameter *edges*
  '( ; alist of *edges* START
    ( ; edge 1 START
      living-room
      (garden west door)
      (attic upstairs ladder)
    ) ; edge 1 END
    ( ; edge 2 START
      garden
      (living-room east door)
    ) ; edge 2 END
    ( ; edge 3 START
      attic
      (living-room downstairs ladder)
    ) ; edge 3 END
  ) ; alist of *edges* END
) ; defparameter *edges* END

;; Backquote ` is used for quasiquoting, allowing code to be embedded into a list
;; (caddr '(garden west door) ) returns DOOR
;; (cadr '(garden west door) ) returns WEST
;; (describe-path '(garden west door) ) returns
;; (THERE IS A DOOR GOING WEST FROM HERE.)
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)
) ; defun describe-path END

;; (assoc 'living-room *edges*) returns
;; (LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;; (cdr (assoc 'living-room *edges*) ) returns
;; ((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;; (mapcar #'describe-path (cdr (assoc 'living-room *edges*) ) ) returns
;; ((THERE IS A DOOR GOING WEST FROM HERE.) (THERE IS A LADDER GOING UPSTAIRS FROM HERE.))
;; (apply #'append (mapcar #'describe-path (cdr (assoc 'living-room *edges*) ) ) ) returns
;; (THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE.)
;; "assoc" selects data without the key from *edges* alist, returns another alist,
;; "cdr" selects data from that alist, returns list with nested lists,
;; for each nested list "mapcar" runs "describe-path", returns list with nested lists
;; "apply" tranforms list into single symbols and runs "append" with those symbols as args
;; "apply" goes 1 level deep in nested loops: (apply #'append '( (foo (bar) ) (abc xyz) ) )
;; returns (FOO (BAR) ABC XYZ)
;; "append" combines all it's arguments into a single list and returns it
;; call-arguments-limit in "clisp" is 4096
;; "mapcar" and "append" are "higher-order functions", take functions as parameters/args
(defun describe-paths (location edges)
  (apply
    (function append) ; 1st argument to function "apply", short version is #'append
    (mapcar ; 2nd argument to function "apply"
      (function describe-path) ; 1st argument to function "mapcar", short: #'describe-path
      (cdr ; 2nd argument to function "mapcar"
        (assoc location edges) ; an argument to function "cdr"
      ) ; cdr END
    ) ; mapcar END
  ) ; apply END
) ; defun describe-paths END

(defparameter *objects* '(whiskey bucket frog chain) )

(defparameter *object-locations*
  '( ; alist of *object-locations* START
    (whiskey living-room)
    (bucket living-room)
    (frog garden)
    (chain garden)
  ) ; alist of *object-locations* END
) ; defparameter *object-locations*

;; "remove-if-not" applies a function to every item in the list, if a function
;; returns True, "remove-if-not" adds that item to a new list, returns new list
(defun objects-at (loc objs obj-locs)
  (labels
    ( ; function definitions START
      (at-loc-p (obj) ; function that returns T or nil is called "predictate" (like oddp)
        (eq
          (cadr ; 1st argument to function "eq"
            (assoc obj obj-locs)
          ) ; cadr END
          loc ; 2nd argument to function "eq"
        ) ; eq END
      ) ; at-loc-p END
    ) ; function definitions END
    (remove-if-not ; (remove-if-not #'oddp '(1 2 3 4 5 6 7 8 9) )
      (function at-loc-p) ; 1st argument to function "remove-if-not", short: #'at-loc-p
      objs ; 2nd argument to function "remove-if-not"
    ) ; remove-if-not END
  ) ; labels END
) ; defun objects-at END

(defun describe-objects (loc objs obj-locs)
  (labels
    ( ; function definitions START
      (describe-obj (obj)
        `(you see a ,obj on the floor.)
      ) ; describe-obj END
    ) ; function definitions END
    (apply
      (function append) ; 1st argument to function "append"
      (mapcar ; 2nd argument to function "append"
        (function describe-obj) ; 1st argument to function "mapcar"
        (objects-at loc objs obj-locs) ; 2nd argument to function "mapcar"
      ) ; mapcar END
    ) ; apply END
  ) ; labels END
) ; defun describe-objects END

(defparameter *location* 'living-room)

(defun look ()
  (append
    (describe-location *location* *nodes*)
    (describe-paths *location* *edges*)
    (describe-objects *location* *objects* *object-locations*)
  ) ; append END
) ; defun look END

(defun walk (direction)
  (let
    ( ; variable definitions START
      (next
        ; "find" searches for an item in a nested lists in a list, returns that item
        (find direction ; argument to function "next"
          (cdr ; argument to function "find"
            (assoc *location* *edges*)
          ) ; cdr END
          :key (function cadr) ; (cadr '(garden west door)) returns WEST
          ; (find 'y '( (5 x) (3 y) (7 z) ) :key #'cadr) returns (3 Y)
        ) ; find END
      ) ; next END
    ) ; variable definitions END
    (if next
      (progn ; true
        (setf *location* ; 1st argument to function "progn"
          (car next)
        ) ; setf *location* END
        (look) ; 2nd argument to function "progn"
      ) ; progn END
      '(you cannot go that way.) ; false
    ) ; if next END
  ) ; let END
) ; defun walk END

;; (pickup 'whiskey) add (WHISKEY BODY) the beginning of *object-locations*
(defun pickup (object)
  (cond
    ( ; condition START
      (member object ; condition, "member" returns a 'tail' of list or NIL
        (objects-at *location* *objects* *object-locations*) ; arg to function "member"
      ) ; member END
      (push ; if condition returns T (non-empty list equals True), then execute commands:
        (list object 'body) ; 1st argument to function "push"
        *object-locations* ; 2nd argument to function "push"
      ) ; push END
      `(you are now carrying the ,object) ; if condition returns T, return this list
    ) ; condition END
    ( ; default condition START
      t ; constant containing T (everything other than empty list '() is true in)
      '(you cannot get that.) ; if condition returns NIL, return this list
    ) ; default condition END
  ) ; cond END
) ; defun pickup END

(defun inventory ()
  (cons
    'items- ; 1st argument to function "cons"
    (objects-at 'body *objects* *object-locations*) ; 2nd argument to function "cons"
  ) ; cons END
) ; defun inventory END

;;;; CHAPTER 6 START ;;;;

;; custom REPL for the game (Read Evaluate Print Loop)
(defun game-repl ()
  (let
    ( ; variable definitions START
      (cmd
        (game-read)
      ) ; cmd END
    ) ; variable definitions END 
    (unless
      (eq
        (car cmd)
        'quit
      ) ; eq END
      (game-print
        (game-eval cmd)
      ) ; game-print END
      (game-repl)
    ) ; unless END
  ) ; let END
) ; defun game-repl END

;; custom read function
(defun game-read ()
  (let
    ( ; variable definitions START
      (cmd
        (read-from-string ; reads 1st word as a symbol, or (string) as a list
          (concatenate 'string "(" (read-line) ")" )
        ) ; read-from-string END
      ) ; cmd END
    ) ; variable definitions END
    (flet
      ( ; function definitions START
        (quote-it (x)
          (list 'quote x)
        ) ; quote-it END
      ) ; function definitions END
      (cons
        (car cmd)
        (mapcar
          (function quote-it)
          (cdr cmd)
        ) ; mapcar END
      ) ; cons END
    ) ; flet END
  ) ; let END
) ; defun game-read END

;; define allowed commands
(defparameter *allowed-commands* '(look walk pickup inventory clear clear0 cls))

;; custom eval function
(defun game-eval (sexp)
  (if
    (member
      (car sexp)
      *allowed-commands*
    ) ; member END
    (eval sexp) ; true
    '(i do not know that command.) ; false
  ) ; if END
) ; defun game-eval END

;; custom print helper function
(defun tweak-text (lst caps lit) ; (listOfChars t nil)
  (when lst
    (let
      ( ; variable definitions START
        (item
          (car lst)
        ) ; item END
        (rest
          (cdr lst)
        ) ; rest END
      ) ; variable definitions END
      (cond
        ( ; 1st condition START
          (eql
            item
            #\space
          ) ; eql END
          (cons
            item
            (tweak-text rest caps lit)
          ) ; cons END
        ) ; 1st condition END
        ( ; 2nd condition START
          (member
            item
            '(#\! #\? #\.)
          ) ; member END
          (cons
            item
            (tweak-text rest t lit)
          ) ; cons END
        ) ; 2nd condition END
        ( ; 3rd condition START
          (eql
            item
            #\"
          ) ; eql END
          (tweak-text rest caps (not lit))
        ) ; 3rd condition END
        ( ; 4th condition START
          lit
          (cons
            item
            (tweak-text rest nil lit)
          ) ; cons END
        ) ; lit END (4th condition END)
        ( ; 5th condition START
          caps
          (cons
            (char-upcase item)
            (tweak-text rest nil lit)
          ) ; cons END
        ) ; 5th condition END
        ( ; default condition START
          t ; constant containing T (everything other than empty list '() is true)
          (cons
            (char-downcase item)
            (tweak-text rest caps lit)
          ) ; cons END
        ) ; default condition END
      ) ; cond END
    ) ; let END
  ) ; when END
) ; defun tweak-text END

;; custom print function
(defun game-print (lst)
  (princ
    (coerce ; converts a list of characters to a string
      (tweak-text
        (coerce ; converts a string to a list of characters
          (string-trim
            "() "
            (prin1-to-string lst) ; returns result as a string
          ) ; string-trim END
          'list
        ) ; coerce 'list END
        t ; caps
        nil ; lit
      ) ; tweak-text END
      'string
    ) ; coerce 'string END
  ) ; princ END
  (fresh-line)
) ; defun game-print END

