;;;; run with sbcl --script game.lisp
;;;; or with clisp game.lisp

;;;; for interactive repl, sbcl --load game.lisp
;;;;                       clisp -i game.lisp
;;;; type (game-repl) to play

(defparameter *nodes* 
  '((living-room (you are in the living-room.
        a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
        there is a well in front of you.))
    (attic (you are in the attic.
        there is a giant welding torch in the corner.))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defparameter *location* 'living-room)

(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;; loc: a list with a single element, e.g. (current-location)
;;; objs: a list of objects, e.g. (fork desk)
;;; obj-locs: An alist of (obj loc) pairs, e.g. ((fork kitchen) (desk bedroom))
(defun objects-at (loc objs obj-locs)
   (labels ((at-loc-p (obj)
              (eq (cadr (assoc obj obj-locs)) loc)))
     (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
               `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;; Look around, look around, how lucky we are to be alive right now
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;;; walka walka walka
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
  (if next
      (progn (setf *location* (car next))
             (look))
      '(you cannot go that way.))))

;;; This function adds an object to 'body but doesn't remove it from the original location
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
           `(you are now carrying the ,object))
          (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun game-read ()
    (let ((cmd (read-from-string
                     (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                         (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun tweak-text (lst caps lit)
   (when lst
  (let ((item (car lst))
         (rest (cdr lst)))
   (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
         ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
         ((eq item #\") (tweak-text rest caps (not lit)))
           (lit (cons item (tweak-text rest nil lit)))
         ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
         (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
 (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                            nil)
                 'string))
  (fresh-line))

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(print (look))