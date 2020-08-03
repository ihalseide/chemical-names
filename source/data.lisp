;;;; data.lisp
;;;; For reading in data from the data files.

(in-package :com.div0.chemical-names)

(defparameter *elements-file* "assets/elements.txt")

(defparameter *compounds-file* "assets/compounds.txt")

;; The first element is nil as a default value, and there's no need to get index 0
(defparameter *numeric-prefixes*
  '(nil mono di tri tetra penta hexa septa octa nona deca)
  "A few prefixes for elements.")

(defparameter *elements* nil
  "A list of the chemical elements on the periodic table.")

(defparameter *compounds* nil
  "A list of the chemical compounds.") 

(defun make-element (atomic-number symbol name name-root period group metal? &optional charges diatomic)
  "Create information for an element"
  (list :atomic-number atomic-number
        :symbol symbol
        :name name
        :name-root name-root
        :period period
        :group group
        :metal? metal?
        :charges (convert-charges charges)
        :diatomic diatomic))

(defun make-compound (symbol name &optional name-root (charge 0))
  "Create information for a compound"
  (list :symbol symbol
        :name name
        :name-root name-root
        :charge (convert-charges charge)))

(defun convert-charges (charges)
  "Convert the raw data symbols of `charges`, which are like '1+' or '(2+ 3+)' into lists of integers."
  (labels ((as-string (symbol-or-number)
             ;; Convert an object that is either a number or a symbol into
             ;; a string.
             (if (numberp symbol-or-number)
                 (string (digit-char symbol-or-number))
                 (symbol-name symbol-or-number)))
           (convert-charge (item)
             (let* ((name (coerce (as-string item) 'list))
                    (number (butlast name))                                       ; Number is all chars except last
                    (charge (last name)))                                         ; Charge is last char
               ;; Put the charge in front and parse as integer
               (parse-integer (coerce (append charge number) 'string)))))
    ;; Convert each charge, but charges may be a single element, which
    ;; is why it is put through `as-list`
    (mapcar #'convert-charge (as-list charges))))

(defun map-lines (func &optional (stream-in *standard-input*))
  "Collect the result of calling a function on every line in a stream."
  (loop for line = (read-line stream-in nil nil)
        while line collect (funcall func line)))

(defun read-one-liner (string)
  "Read a line into a list."
  (let ((*read-eval* nil)
        (result-list nil)
        (in (make-string-input-stream string)))
    (loop
      (handler-case (push (read in) result-list)
        (end-of-file () (return (nreverse result-list)))))))

(defun line-function (match-function)
  "Return a function the applies the contents of a line to a function."
  (lambda (line)
    (apply match-function (read-one-liner line))))

(defun load-chem-data! ()
  "This loads all of the chemical data stored in files on disk. This must be loaded before any chemical naming will work."
  (with-open-file (in *elements-file*)
    (setf *elements*
          ;; Use a line function to call `make-element`
          (map-lines (line-function #'make-element) in)))
  (with-open-file (in *compounds-file*)
    (setf *compounds*
          ;; Use a line function to call `make-compound`
          (map-lines (line-function #'make-compound) in)))) 

;; Load the data now!
(load-chem-data!)
