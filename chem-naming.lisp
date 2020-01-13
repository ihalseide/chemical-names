;;;; chem-naming.lisp


(in-package :com.div0.chem-naming)


(defparameter *IONS* (make-hash-table))
(defparameter *PREFIXES* (list '(1 "mono")
                               '(2 "di")
                               '(3 "tri")
                               '(4 "tetra")
                               '(5 "penta")
                               '(6 "hexa")
                               '(7 "septa")
                               '(8 "octa")
                               '(9 "nova")
                               '(10 "deca")
                               '(11 "?")
                               '(12 "?")
                               '(13 "?")))

(defun read-upper-char (stream))


(defun read-element-symbol (stream)
  ; USE REGEX?
  (concat (read-char stream)
          (read-char stream)))


(defun read-element (stream &optional (do-read-number t))
  (let ((element-symbol (read-element-symbol stream)))
       (if do-read-number
           (list element-symbol number)
           element-symbol)))


(defun read-ion (stream &optional (do-read-number t)))


(defun is-metal? (ion))


(defun get-name-stem (ion))


(defun get-symbol (ion))


(defun get-name (ion))


(defun get-charges (ion))


(defun roman (number)
    (let ((minimum 1)
          (maximum 20))
        (if (<= minimum num maximum)
            (nth (1- number) 
                 (list "i" "ii" "iii" "iv" "v"
                       "vi" "vii" "viii" "ix" "x"
                       "xi" "xii" "xiii" "xiv" "xv"
                       "xvi" "xvii" "xviii" "xix" "xx"))
            (error (format nil "Number must be between ~a and ~a" minimum maximum)))))


(defun basic-validate-formula (formula))


(defun first-is-hydrogen? (formula))


(defun name-acid (formula))


(defun first-is-metal? (formula))


(defun name-metal-nonmetal (formula))


(defun first-is-nonmetal (formula))


(defun name-nonmetal-nonmetal (formula))


(defun get-number-prefix (number))


(defun add-prefix-to-nonmetal (ion amount &optional (is-first-in-formula nil)))


(defun add-suffix-to-ion (ion suffix))


(defun name-nonmetal (ion amount &optional (is-first-in-formula nil)))


(defun read-ions-csv ())


(defun make-ion ())


(defun read-csv-ion-def (stream))


(defun read-csv-ion-charges (stream))


(defun read-csv-ion-charge (stream))


(defun read-csv-boolean (stream))


(defun name-formula (formula)
    (if (basic-validate-formula formula)
        (cond ((equal formula "H2O") "water")
              ((first-is-hydrogen? formula) (name-acid formula))
              ((first-is-metal? formula) (name-metal-nonmetal formula))
              ((first-is-nonmetal formula) (name-nonmetal-nonmetal formula))
              (t "I don't know that."))
        (error "bad format")))
          
          
(defun read-and-name-formula (&optional (in-stream *IOSTREAM*)
                                        (out-stream *IOSTREAM*)))


(defun read-and-name-formulas (&optional (in-stream *IOSTREAM*)
                                        (out-stream *IOSTREAM*)))
                                        
                                        
(defun print-all-ions ())


(defun load-ions ())


(defun get-ions ())


(load-ions)
