;;;; predicates.lisp

(in-package :com.div0.chemical-names)

(defun atom? (x) (atom x))

(defun list? (x) (listp x))

(defun cons? (x) (consp x))

(defun number? (x) (numberp x))

