;;;; sbcl --script setsvg.lisp > foo.svg

; Milestone 0: print an svg
; Milestone 1: print a circle with a random color

; Initialize the global random state.
; https://stackoverflow.com/questions/4034042/random-in-common-lisp-not-so-random
(setf *random-state* (make-random-state t))

(defparameter *begin-svg* "<svg xmlns=\"http://www.w3.org/2000/svg\"
  xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  width=\"500\" height=\"500\">")

(defparameter *end-svg* "</svg>")

(defun ellipse (color)
  (concatenate 'string "<ellipse
      cx=\"250.0\"
      cy=\"250.0\"
      rx=\"100.0\"
      ry=\"100.0\"
      stroke=\"#fc8d62\"
      stroke-width=\"5\"
      fill=\"#" color "\"
    />")
  )

(defparameter *colors* '("66c2a5" "fc8d62" "8da0cb"))

(defun randselect (somelist)
  (let
    ((n (length somelist)))
    (car (nthcdr (random n) somelist))
  )
)

(defun randcolor ()
  (randselect *colors*)
)

(defun svg (body)
  (format t "~d" *begin-svg*)
  (format t "~d" body)
  (format t "~d" *end-svg*)  
)

(defun randcircle ()
  (svg (ellipse (randcolor)))
  )

(randcircle)
