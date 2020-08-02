;;;; util.lisp
;;;; For more general utilities

(in-package :com.div0.chemical-names)

(defun flatten (x)
  "Turn a nested list structure into a 1-level list of atoms."
  (when x
    (if (or (cons? x) (atom? x))
        (list x)
        (mapcan #'flatten x)))) 

(defun newline-char? (char)
  "Return whether a character represents a newline at all."
  (or (char= #\Newline char)
      (char= #\Linefeed char)
      (char= #\Return char)))

(defun vowel-char? (char)
  "Tell if a character is a vowel. Used for adding endings and prefixes to words properly."
  (member (char-downcase char)
          (list #\a #\e #\i #\o #\u))) 

(defun make-index-list (sequence)
  "Create a list of numbers counting as indices for a sequence."
  (loop for a from 0 upto (1- (length sequence))
        collect a))

(defun as-list (obj)
  "Make sure a given object is a list."
  (if (list? obj)
      obj
      (list obj))) 
