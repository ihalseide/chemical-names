;; macros.lisp

(in-package :com.div0.chemical-names)

(defmacro let1 (var value &body body)
  "For when you only want to let 1 lexical binding."
  `(let ((,var ,value))
     ,@body))
