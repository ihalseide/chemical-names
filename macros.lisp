;; macros.lisp

(in-package :com.div0.chemical-names)

(defun vector-pop-safe (vector sentinel)
  "Try to pop from a vector and return the sentinel value if there is an error."
  ;; This handler case catches popping from an empty vector.
  (handler-case (vector-pop vector)
    (simple-error () sentinel)))

(defmacro do-shrinking-string ((var string) &body body)
  "Loop with a given named string shrinking down until 1 char long."
  (with-gensyms (char)
    `(let ((,var (string->vector ,string)))
       (loop for ,char = nil then (vector-pop-safe ,var nil)
	  while (plusp (length ,var))
	    ,@body))))
