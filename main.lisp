;;;; main.lisp
;;;; Note: do not include in the ASDF file! This is a separate way to load the project.

(cl:in-package :cl-user)

(let ((*default-pathname-defaults* (truename "./dependencies/macro-utils/")))
  (load "main"))

(load "source/packages")
(load "source/macros")
(load "source/predicates")
(load "source/utils")
(load "source/data")
(load "source/input")
(load "source/naming")