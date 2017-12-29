;;;; sbcl --script setsvg.lisp > foo.svg

; Milestone 0: print an svg
; Milestone 1: print a circle with a random color

(defparameter *begin-svg* "<svg xmlns=\"http://www.w3.org/2000/svg\"
  xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  width=\"500\" height=\"500\">")

(defparameter *end-svg* "</svg>")

(defun ellipse (color)
  (concatenate 'string "<ellipse
      cx=\"250.0\"
      cy=\"250.0\"
      rx=\"60.0\"
      ry=\"60.0\"
      stroke=\"#fc8d62\"
      stroke-width=\"5\"
      fill=\"#" color "\"
    />")
  )

(defparameter *colors* '("66c2a5" "fc8d62" "8da0cb"))

(defun randcolor ()
  (let 
    ((n (length *colors*)))
    (car (nthcdr (random n) *colors*))
  )
)

(print (ellipse (randcolor)))

; (format t "~d" *begin-svg*)
; (format t "~d" *oval*)
; (format t "~d" *end-svg*)