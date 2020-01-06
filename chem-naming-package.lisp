;;;; chem-naming-package.lisp
;;; Define the package for chem-naming.lisp


(in-package :cl-user)


(defpackage :com.div0.chem-naming
  (:use :common-lisp)
  (:export :name-formula
           :read-and-name-formula
           :print-all-ions
           :get-ions))
