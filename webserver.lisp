;;;; CHAPTER 13 (Land Of Lisp)

(defun http-char (c1 c2 &optional (default #\Space))
  (let
    ( ; variable definition START
      (code
        (parse-integer
          (coerce (list c1 c2) 'string)
          :radix 16
          :junk-allowed t
        ) ; parse-integer END
      ) ; code END
    ) ; variable definition END
    (if code
      (code-char code)
      default
    ) ; if END
  ) ; let END
) ; defun http-char END

(defun decode-param (s)
  (labels
    ( ; function definition START
      (f (lst)
        (when lst
          (case (car lst)
            ( ; 1st case START
              #\%
              (cons
                (http-char (cadr lst) (caddr lst))
                (f (cdddr lst))
              ) ; cons END
            ) ; 1st case END
            ( ; 2nd case START
              #\+
              (cons
                #\Space
                (f (cdr lst))
              ) ; cons END
            ) ; 2nd case END
            ( ; default case START
              otherwise
              (cons
                (car lst)
                (f (cdr lst))
              ) ; cons END
            ) ; default case END
          ) ; case END
        ) ; when END
      ) ; f END
    ) ; function definition END
    (coerce (f (coerce s 'list)) 'string)
  ) ; labels END
) ; defun decode-param END

;; (decode-param "Color+in+Terminal+%1B[93;100mHello%1b[0m+world%21")

(defun parse-params (s)
  (let*
    ( ; variable definitions START
      (i1 (position #\= s))
      (i2 (position #\& s))
    ); variable definitions END
    (cond
      ( ; 1st condition START
        i1
        (cons
          (cons
            (intern (string-upcase (subseq s 0 i1)))
            (decode-param (subseq s (1+ i1) i2))
          ) ; cons END
          (and
            i2
            (parse-params (subseq s (1+ i2)))
          ) ; and END
        ) ; cons END
      ) ; 1st condition END
      ( ; 2nd condition START
        (equal s "")
        'nil
      ) ; 2nd condition END
      ( ; default condition START
        t
        s
      ) ; default condition END
    ) ; cond END
  ) ; let* END
) ; defun parse-params END

(defun parse-url (s)
  (let*
    ( ; variable definitions START
      (url
        (subseq
          s
          (+ 2 (position #\Space s))
          (position #\Space s :from-end t)
        ) ; subseq END
      ) ; url END
      (x
        (position #\? url)
      ) ; x END
    ) ; variable definitions END
    (if x
      (cons
        (subseq url 0 x)
        (parse-params (subseq url (1+ x)))
      ) ; cons END
      (cons
        url
        '()
      ) ; cons END
    ) ; if END
  ) ; let* END
) ; defun parse-url END

(defun get-header (stream)
  (let*
    ( ; variable definitions START
      (s (read-line stream))
      (h
        (let
          ((i (position #\: s)))
          (when i
            (cons
              (intern (string-upcase (subseq s 0 i)))
              (subseq s (+ i 2))
            ) ; cons END
          ) ; when END
        ) ; let END
      ) ; h END
    ) ; variable definitions END
    (when h
      (cons h (get-header stream))
    ) ; when END
  ) ; let* END
) ; defun get-header END

(defun get-content-params (stream header)
  (let
    ((length (cdr (assoc 'content-length header))))
    (when length
      (let
        ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)
      ) ; let END
    ) ; when END
  ) ; let END
) ; defun get-content-params END

#+sbcl 
  (require :sb-bsd-sockets) 

(defun serve (request-handler)
  (let
    ( ; variable devinition START
      #+clisp
        (socket (socket-server 8080))
      #+sbcl
        (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)) 
      #+clozure 
        (socket (make-socket :connect :passive :local-port 8080 :keepalive t :reuse-address t))
    ) ; variable devinition END
    #+sbcl
      (progn
        (setf (sb-bsd-sockets:sockopt-keep-alive socket) t)
        (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
        (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 8080)
        (sb-bsd-sockets:socket-listen socket 5)
      ) ; progn END
    (unwind-protect
      (loop
        named lispserver
        do
          (with-open-stream
            #+clisp
              (stream (socket-accept socket))
            #+sbcl
              (stream (sb-bsd-sockets:socket-make-stream (sb-bsd-sockets:socket-accept socket) :input t :output t))
            #+clozure 
              (stream (accept-connection socket))
            (let*
              ( ; variable definitions START
                (url (parse-url (read-line stream)))
                (path (car url))
                (header (get-header stream))
                (params
                  (append
                    (cdr url)
                    (get-content-params stream header)
                  ) ; append END
                ) ; params END
                (*standard-output* stream)
              ) ; variable definitions END
              (when
                (eq (funcall request-handler path header params) 'shutdown)
                (return-from lispserver 'shutdown_lisp_server)
              ) ; when END
            ) ; let * END
          ) ; with-open-stream END
      ) ; loop END
      #+clisp
        (socket-server-close socket)
      #+sbcl
        (sb-bsd-sockets:socket-close socket :abort t)
      #+clozure 
        (close socket :abort t)
    ) ; unwind-protect END
  ) ; let END
) ; defun serve END

; (load "dice_of_doom_v3.lisp")
; (serve #'dod-request-handler)
; http://localhost:8080/game.html

