;;;; CHAPTER 7 (Land Of Lisp)

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

;; lists in Lisp are built out of CONS CELLS - they link together 2 pieces of data.
;; the right slot in the last CONS CELL in a list should contain a NIL.
;; '() - empty list, aliases: (), 'NIL, NIL - (cons 'FOO (cons 'BAR 'NIL))

(defparameter *wizard-nodes*
  '( ; alist of *wizard-nodes* START
    ( ; node 1 START
      living-room
      (you are in the living-room. a wizard is snoring loudly on the couch.)
    ) ; node 1 END
    ( ; node 2 START
      garden
      (you are in a beautiful garden. there is a well in front of you.)
    ) ; node 2 END
    ( ; node 3 START
      attic
      (you are in the attic. there is a giant welding torch in the corner.)
    ) ; node 3 END
  ) ; alist of *wizard-nodes* END
) ; defparameter *wizard-nodes* END

(defparameter *wizard-edges*
  '( ; alist of *wizard-edges* START
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
  ) ; alist of *wizard-edges* END
) ; defparameter *wizard-edges* END

;; graphviz has a DOT format, converting node identifiers into DOT identifiers
;; DOT format contains only letters, digits, underscores; substitute others with _
(defun dot-name (exp)
  (substitute-if ; substiture-if-not is deprecated and should not be used
    #\_
    (complement #'alphanumericp) ; create predictate with needed behavior: non-alnum
    (prin1-to-string exp)
  ) ; substiture-if END
) ; defun dot-name END

;; generating label for the node
(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
    (let
      ( ; variable definitions START
        (s (write-to-string exp :pretty nil))
      ) ; variable definitions END
      (if
        (> (length s) *max-label-length*)
        (concatenate
          'string
          (subseq
            s
            0
            (- *max-label-length* 3)
          ) ; subseq END
          "..."
        ) ; concatenate END
        s
      ) ; if END
    ) ; let END
    ""
  ) ; if exp END
) ; defun dot-label END

;; generating DOT information for the nodes
(defun nodes->dot (nodes)
  (mapc
    (lambda (node)
      (fresh-line)
      (princ (dot-name (car node)))
      (princ "[label=\"")
      (princ (dot-label node))
      (princ "\"];")
    ) ; lambda END
    nodes
  ) ; mapc END
) ; defun nodes->dot END

;; generate DOT information for the edges
(defun edges->dot (edges)
  (mapc
    (lambda (node)
    (mapc
      (lambda (edge)
        (fresh-line)
        (princ (dot-name (car node)))
        (princ "->")
        (princ (dot-name (car edge)))
        (princ "[label=\"")
        (princ (dot-label (cdr edge)))
        (princ "\"];")
      ) ; lambda (edge) END
      (cdr node)
    ) ; mapc END
    ) ; lambda (node) END
    edges
  ) ; mapc END
) ; defun edges->dot END

;; generating all DOT data
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}")
  (fresh-line)
) ; defun graph->dot END

;; Turning the DOT file into a picture
(defun dot->png (fname thunk)
  (with-open-file
    ( ; parameters START
      *standard-output*
      fname
      :direction :output
      :if-exists :supersede
    ) ; parameters END
    (funcall thunk)
  ) ; with-open-file END
  #+clisp
    (ext:shell (concatenate 'string "dot -Tpng -O " fname))
  #+sbcl 
    (sb-ext:run-program "/usr/bin/dot" (list "-Tpng" "-O" fname) :input 'nil :output *standard-output*)
  #+clozure 
    (ccl:run-program "/usr/bin/dot" (list "-Tpng" "-O" fname) :input 'nil :output *standard-output*)
) ; defun dot->png END

;; creating a picture of a graph
(defun graph->png (fname nodes edges)
  (dot->png
    fname
    (lambda ()
      (graph->dot nodes edges)
    ) ; lambda END
  ) ; dot->png END
) ; defun graph->png END

;;;; (graph->png "wizard.dot" *wizard-nodes* *wizard-edges*)

;; undirected graphs
(defun uedges->dot (edges)
  (maplist
    (lambda (lst)
      (mapc
        (lambda (edge)
          (unless
            (assoc
              (car edge)
              (cdr lst)
            ) ; assoc END
            (fresh-line)
            (princ (dot-name (caar lst)))
            (princ "--")
            (princ (dot-name (car edge)))
            (princ "[label=\"")
            (princ (dot-label (cdr edge)))
            (princ "\"];")
          ) ; unless END
        ) ; lambda (edge) END
        (cdar lst)
      ) ; mapc END
    ) ; lambda (lst) END
    edges
  ) ; maplist END
) ; defun uedges->dot END

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}")
  (fresh-line)
) ; defun ugraph->dot END

(defun ugraph->png (fname nodes edges)
  (dot->png fname
    (lambda ()
      (ugraph->dot nodes edges)
    ) ; lambda END
  ) ; dot->png END
) ; defun ugraph->png END

;;;; (ugraph->png "uwizard.dot" *wizard-nodes* *wizard-edges*)

