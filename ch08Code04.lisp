;;;; CHAPTER 8 (Land Of Lisp)

;;;; Speeding up Wumpus game by using hash table instead of alist.
;;;; Added function: hash-edges
;;;; Replaced function: get-connected with get-connected-hash
;;;; Changed function: find-islands
;;;; Added color to the graph: changed functions: nodes->dot, uedges->dot

;; clear screen (Unix-like systems) START
(defun clear ()
"Clears screen on Unix-like systems."
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
;; Modern terminal emulators only:
(defparameter clear (format nil "~c[3J~c[1;1H~c[0J" #\Esc #\Esc #\Esc))
(defparameter clear0 clear)
(defparameter cls clear)
;; clear screen (Unix-like systems) END

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
      (if (> (length s) *max-label-length*)
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
      (let
        ( ; variable definition START
          (l (dot-label node))
        ) ; variable definition END
        (princ "[label=\"")
        (princ l)
        (if (eql (parse-integer l :start 1 :junk-allowed t) *player-pos*)
          (princ "\",color=darkolivegreen,style=filled,fillcolor=darkolivegreen,fontcolor=gray100];")
          (princ "\"];")
        ) ; if END
      ) ; let END
    ) ; lambda END
    nodes
  ) ; mapc END
) ; defun nodes->dot END

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
            (let
              ( ; variable definition START
                (n1 (dot-name (caar lst)))
                (n2 (dot-name (car edge)))
              ) ; variable definition END
              (princ n1)
              (princ "--")
              (princ n2)
              (princ "[label=\"")
              (princ (dot-label (cdr edge)))
              (if
                (or
                  (eql (parse-integer n1) *player-pos*)
                  (eql (parse-integer n2) *player-pos*)
                ) ; or END
                (princ "\",penwidth=3.0,color=darkolivegreen];")
                (princ "\"];")
              ) ; if END
            ) ; let END
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

; (ugraph->png "uwizard" *wizard-nodes* *wizard-edges*)
; dot -Tpng city.dot -o city.dot.png
; dot -Tsvg city.dot -o city.dot.svg

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*))
) ; defun random-node END

(defun edge-pair (a b)
  (unless
    (eql a b)
    (list (cons a b) (cons b a))
  ) ; unless END
) ; defun edge-pair END

(defun make-edge-list ()
  (apply
    #'append
    (loop
      repeat *edge-num*
      collect (edge-pair (random-node) (random-node))
    ) ; loop END
  ) ; apply END
) ; defun make-edge-list END

(defun hash-edges (edge-list)
  (let
    ((tab (make-hash-table)))
    (mapc
      (lambda (x)
        (let
          ((node (car x)))
          (push (cdr x) (gethash node tab))
        ) ; let END
      ) ; lambda END
      edge-list
    ) ; mapc END
    tab
  ) ; let END
) ; defun hash-edges END

(defun direct-edges (node edge-list)
  (remove-if ; remove-if-not is deprecated by ANSI standard, use complement
    (complement
      (lambda (x)
        (eql (car x) node)
      ) ; lambda END
    ) ; complement END
    edge-list
  ) ; remove-if END
) ; defun direct-edges END

(defun get-connected-hash (node edge-tab)
  (let
    ( ; variable definition START
      (visited (make-hash-table))
    ) ; variable definition END
    (labels
      ( ; function definition START
        (traverse (node)
          (unless
            (gethash node visited)
            (setf (gethash node visited) t)
            (mapc
              (lambda (edge) (traverse edge))
              (gethash node edge-tab)
            ) ; mapc END
          ) ; unless END
        ) ; traverse (node) END
      ) ; function definition END
      (traverse node)
    ) ; labels END
    visited
  ) ; let END
) ; defun get-connected-hash END

;; (time (dotimes (i 100) (get-connected-hash 1 (hash-edges (make-edge-list)))))

(defun connect-with-bridges (islands)
  (when
    (cdr islands)
    (append
      (edge-pair (caar islands) (caadr islands))
      (connect-with-bridges (cdr islands))
    ) ; append END
  ) ; when END
) ; defun connect-with-bridges END

(defun find-islands (nodes edge-list)
  (let
    ((islands nil))
    (labels
      ( ; function definition START
        (find-island (nodes)
          (let*
            ( ; variable definitions START
              (connected
                (loop
                  for key being the hash-keys of
                    (get-connected-hash (car nodes) (hash-edges edge-list))
                  collect key
                ) ; loop END
              ) ; definition of variable "connected" END
              (unconnected (set-difference nodes connected))
            ) ; variable definitions END
            (push connected islands)
            (when
              unconnected
              (find-island unconnected)
            ) ; when END
          ) ; let* END
        ) ; find-island (nodes) END
      ) ; function definition END
      (find-island nodes)
    ) ; labels END
    islands
  ) ; let END
) ; defun find-islands END

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list)
) ; defun connect-all-islands END

(defun edges-to-alist (edge-list)
  (mapcar
    (lambda (node1)
      (cons
        node1
        (mapcar
          (lambda (edge) (list (cdr edge)))
          (remove-duplicates (direct-edges node1 edge-list) :test #'equal)
        ) ; mapcar END
      ) ; cons END
    ) ; lambda (node1) END
    (remove-duplicates (mapcar #'car edge-list))
  ) ; mapcar END
) ; defun edges-to-alist END

(defun add-cops (edge-alist edges-with-cops)
  (mapcar
    (lambda (x)
      (let
        ( ; variable definitions START
          (node1 (car x))
          (node1-edges (cdr x))
        ) ; variable definitions END
        (cons
          node1
          (mapcar
            (lambda (edge)
              (let
                ((node2 (car edge)))
                (if (intersection (edge-pair node1 node2) edges-with-cops :test #'equal)
                  (list node2 'cops)
                  edge
                ) ; if END
              ) ; let END
            ) ; lambda (edge) END
            node1-edges
          ) ; mapcar END
        ) ; cons END
      ) ; let END
    ) ; lambda (x) END
    edge-alist
  ) ; mapcar END
) ; defun add-cops END

(defun make-city-edges ()
  (let*
    ( ; variable definitions START
      (nodes (loop for i from 1 to *node-num* collect i))
      (edge-list (connect-all-islands nodes (make-edge-list)))
      (cops (remove-if (complement (lambda (x) (zerop (random *cop-odds*)))) edge-list))
    ) ; variable definitions END
    (add-cops (edges-to-alist edge-list) cops)
  ) ; let* END
) ; defun make-city-edges END

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist)))
) ; defun neighbors END

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist))
) ; defun within-one END

(defun within-two (a b edge-alist)
  (or
    (within-one a b edge-alist)
    (some
      (lambda (x) (within-one x b edge-alist))
      (neighbors a edge-alist)
    ) ; some END
  ) ; or END
) ; defun within-two END

(defun make-city-nodes (edge-alist)
  (let
    ( ; variable definitions START
      (wumpus (random-node))
      (glow-worms (loop for i below *worm-num* collect (random-node)))
    ) ; variable definitions END
    (loop
      for n from 1 to *node-num*
      collect
        (append
          (list n)
          (cond
            ((eql n wumpus) '(wumpus))
            ((within-two n wumpus edge-alist) '(blood!))
          ) ; cond END
          (cond
            ((member n glow-worms) '(glow-worm))
            ((some (lambda (worm) (within-one n worm edge-alist)) glow-worms) '(lights!))
          ) ; cond END
          (when
            (some #'cdr (cdr (assoc n edge-alist))) '(sirens!)
          ) ; when END
        ) ; append END
    ) ; loop END
  ) ; let END
) ; defun make-city-nodes END

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
) ; defun new-game END

(defun find-empty-node ()
  (let
    ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
      (find-empty-node)
      x
    ) ; if END
  ) ; let END
) ; defun find-empty-node END

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*)
) ; defun draw-city END


;;;; DRAWING ONLY KNOWN PARTS OF THE CITY
(defun known-city-nodes ()
  (mapcar
    (lambda (node)
      (if (member node *visited-nodes*)
        (let
          ((n (assoc node *congestion-city-nodes*)))
          (if (eql node *player-pos*)
            (append n '(*))
            n
          ) ; if END
        ) ; let END
        (list node '?)
      ) ; if END
    ) ; lambda END
    (remove-duplicates 
      (append
        *visited-nodes*
        (mapcan (lambda (node) (neighbors node *congestion-city-edges*)) *visited-nodes*)
      ) ; append END
    ) ; remove-duplicates END
  ) ; mapcar END
) ; defun known-city-nodes END

(defun known-city-edges ()
  (mapcar
    (lambda (node)
      (cons
        node
        (mapcar
          (lambda (x)
            (if (member (car x) *visited-nodes*)
              x
              (list (car x))
            ) ; if END
          ) ; lambda (x) END
          (cdr (assoc node *congestion-city-edges*))
        ) ; mapcar END
      ) ; cons END
    ) ; lambda (node) END
    *visited-nodes*
  ) ; mapcar END
) ; defun known-city-edges END

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges))
) ; defun draw-known-city END

; redefining function new-game
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city)
) ; defun new-game END

(defun walk (pos)
  (handle-direction pos nil)
) ; defun walk END

(defun charge (pos)
  (handle-direction pos t)
) ; defun charge END

(defun handle-direction (pos charging)
  (let
    ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
      (handle-new-place edge pos charging)
      (princ "That location does not exist!")
    ) ; if END
  ) ; let END
) ; defun handle-direction END

(defun handle-new-place (edge pos charging)
  (let*
    ( ; variable definitions START
      (node (assoc pos *congestion-city-nodes*))
      (has-worm (and (member 'glow-worm node) (not (member pos *visited-nodes*))))
    ) ; variable definitions END
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond
      ( ; 1st condition START
        (member 'cops edge)
        (princ "You ran into the cops. Game Over.")
      ) ; 1st condition END
      ( ; 2nd condition START
        (member 'wumpus node)
        (if charging
          (princ "You found the Wumpus! You win!")
          (princ "You ran into the Wumpus. Game Over.")
        ) ; if END
      ) ; 2nd condition END
      ( ; 3rd condition START
        charging (princ "You wasted your last bullet. Game Over.")
      ) ; 3rd condition END
      ( ; 4th condition START
        has-worm
        (let
          ((new-pos (random-node)))
          (princ "You ran into a Glow Worm Gang! You're now at ")
          (princ new-pos)
          (fresh-line)
          (handle-new-place nil new-pos nil)
        ) ; let END
      ) ; 4th condition END
      ( ; default condition START
        t
        node
      ) ; default condition END
    ) ; cond END
  ) ; let* END
) ; defun handle-new-place END

