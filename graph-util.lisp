;;;; Utilities library for Land Of Lisp

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
      (concatenate 'string fname ".dot")
      :direction :output
      :if-exists :supersede
    ) ; parameters END
    (funcall thunk)
  ) ; with-open-file END
  #+clisp
    (ext:shell (concatenate 'string "dot -Tpng -O " fname ".dot"))
  #+sbcl 
    (sb-ext:run-program "/usr/bin/dot" (list "-Tpng" "-O" (concatenate 'string fname ".dot")) :input 'nil :output *standard-output*)
  #+clozure 
    (ccl:run-program "/usr/bin/dot" (list "-Tpng" "-O" (concatenate 'string fname ".dot")) :input 'nil :output *standard-output*)
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

;;;; (graph->png "wizard" *wizard-nodes* *wizard-edges*)

;; undirected graphs
(defun uedges->dot (edges)
  (mapl
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
  ) ; mapl END
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

;;;; (ugraph->png "uwizard" *wizard-nodes* *wizard-edges*)

