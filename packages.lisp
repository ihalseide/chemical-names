;;;; packages.lisp

(in-package :cl-user)

(defpackage :com.div0.chemical-names
  (:use :common-lisp 
        :com.div0.macro-utils)
  (:export :name->formula
           :formula->name
           :load-chem-data))
