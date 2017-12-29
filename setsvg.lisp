;;;; sbcl --script setsvg.lisp > foo.svg

(defparameter *begin-svg* "<svg xmlns=\"http://www.w3.org/2000/svg\"
  xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  width=\"500\" height=\"500\">")

(defparameter *end-svg* "</svg>")

(defparameter *oval* "<ellipse
      cx=\"220.0\"
      cy=\"130.0\"
      rx=\"10.0\"
      ry=\"10.0\"
      stroke=\"#fc8d62\"
      stroke-width=\"5\"
      fill=\"#fc8d62\"
    />")

(format t "~d" *begin-svg*)
(format t "~d" *oval*)
(format t "~d" *end-svg*)