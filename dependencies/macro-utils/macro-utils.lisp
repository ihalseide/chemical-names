;;;; macro-utils.lisp

(in-package :com.div0.macro-utils)

(defmacro with-gensyms ((&rest names) &body body)
  "Let a bunch of named gensyms."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
