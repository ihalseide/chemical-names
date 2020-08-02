;;;; macros.lisp

(in-package :com.div0.chemical-names)

(defmacro let1 (var value &body body)
  "For when you only want to let 1 lexical binding."
  `(let ((,var ,value))
     ,@body))

(defmacro lets (bindings &body body)
  "Create a lexical binding like `let` but without parantheses surrounding each binding."
  `(let ,(collect-pairs bindings)
     ,@body))

(defmacro lets* (bindings &body body)
  "Create a lexical binding like `let*` but without parentheses surrounding each binding."
  `(let* ,(collect-pairs bindings)
     ,@body))

;; This is a helper function for `lets` and `lets*`
(defun collect-pairs (list)
  "Build a list up of lists that have 2 items from a given (usually flat) list."
  (flet ((f (list)
           (when list
             ;; Build up a list of a pair and by recursing on the rest...
             (cons (list (car list) (cdar list)) ; Get the next 2 items
                   (collect-pairs (cddr list))))))
    (f list)))   ; Recurse on the rest of the list
