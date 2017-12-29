; returns "foobar"
; it's a generic function so you have to pass the return type ???
; http://cl-cookbook.sourceforge.net/strings.html#concat
(concatenate 'string "foo" "bar")

; Initialize the global random state.
; https://stackoverflow.com/questions/4034042/random-in-common-lisp-not-so-random
(setf *random-state* (make-random-state t))

(defun randselect (somelist)
  (let
    ((n (length somelist)))
    (car (nthcdr (random n) somelist))
  )
)