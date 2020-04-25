(in-package #:com.div0.chemical-names)

(defparameter *elements* nil
  "List of all of the elements")

(defparameter *compounds* nil
  "List of all of the compounds available.")

(defparameter *chemical-exceptions* nil
  "List of chemical formulas and names that an exception is made for.")

(defun make-element-def (atomic-number symbol name period group is-metal charges)
  (list :atomic-number atomic-number
	:symbol symbol
	:name name
	:period period
	:group group
	:is-metal is-metal
	:charges charges))

(defun make-compound-def (symbol name name-root charge)
  (list :symbol symbol
	:name name
	:name-root name-root
	:charge charge))

(defun make-name-def (formula name)
  (list :formula formula
	:name name))

(defun make-compound (symbol count)
  (list :symbol symbol
	:count count))

(defun parse-charge-list (seq)
  (map 'list #'parse-charge seq))

(defun parse-compound-def (string)
  "Parse a line in a compounds file."
  (destructuring-bind (symbol name name-root charge)
      (split-string string)
    (make-compound-def symbol
		       name
		       (unless (string-equal name-root "x") name-root)
		       (parse-charge charge))))

(defun parse-charge (string)
  "Turn a charge like ' 1+ ' into an int ( +1 )."
  (with-input-from-string (in string)
    (let ((value (parse-integer (read-while #'digit-char-p in)))
	  (sign (read-char in nil 'eof)))
      (if (= value 0) 0
	  (cond ((and (characterp sign) (char= sign #\+)) value)
		((and (characterp sign) (char= sign #\-)) (- value))
		(t (error "Found ~S after charge magnitude instead of a sign." sign)))))))
      
(defun load-elements (filepath)
  "Load all of the element data into *elements* (destructive)."
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
       while line
       if (char/= (elt line 0) #\;)
       collect (parse-element-def line))))

(defun parse-element-def (string)
  "Parse a line in a elements file."
  (destructuring-bind (atomic-number symbol name period
				     group metal &rest charges)
      (split-string string)
    (make-element-def (parse-integer atomic-number)
		      symbol
		      name
		      (parse-integer period)
		      (parse-integer group)
		      (is-metal metal)
		      (parse-charge-list charges))))

(defun is-metal (string)
  (string-equal string "metal"))

(defun read-while (test &optional (stream *standard-input*))
  "Read characters from a stream as long as the test passes on the characters."
   (with-output-to-string (out)
     (loop for c = (peek-char nil stream nil nil)
	while (and c (funcall test c))
	do (write-char (read-char stream) out))))

(defun whitespace-char-p (char)
  (or (char= #\Space char)
      (not (graphic-char-p char))))

(defun read-word (&optional (stream *standard-input*))
  "Read a word delimited by WHITESPACE-CHAR-P."
  (read-while (complement #'whitespace-char-p) stream))

(defun split-string (string &optional (test #'whitespace-char-p))
  "Split a string on characters that pass the given test."
  (with-input-from-string (stream string)
    (loop for subseq = (read-while (complement test) stream)
       when (plusp (length subseq)) collect subseq
       until (zerop (length (read-while test stream))))))

(defun split-string-on (string &rest split-chars)
  "Split a string on all of the characters given."
  (flet ((do-split (char)
	   (find char split-chars)))
    (split-string string #'do-split)))

(defun mklist (x)
  "Shortcut to turn 1 thing into a 1 element list."
  (if (listp x) x (list x)))

(defun formula->name (formula-str)
  (let ((match (find formula-str *chemical-exceptions*
		     :key (lambda (entry) (first entry)))))
    (cond (match (second match))
	  (t
	   "Unknown."))))

(defun load-chem-info ()
  (setf *elements* (load-elements "elements.txt"))
  (setf *compounds* (load-compounds "compounds.txt"))
  (setf *chemical-exceptions* (load-chem-exceptions "exceptions.txt")))

(defun load-compounds (filepath)
  "Load all of the element compounds data."
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
       while line
       if (char/= (elt line 0) #\;)
       collect (parse-compound-def line))))

(defun load-chem-exceptions (filepath)
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
       while line
       if (char/= (elt line 0) #\;)
       collect (parse-name-def line))))

(defun parse-name-def (string)
  (destructuring-bind (formula name) (split-string string)
    (make-name-def formula name)))

(defun name-ionic-compound (ionic-compound)
  (let ((ions (read-compounds ionic-compound)))
    (if (> (length ions) 2)
	(error "Ionic compound may only have 2 ions."))
    (let ((cation (name-ion (first ions)))
	  (anion (name-ion (second ions))))
      (format nil "~a ~a" cation anion))))

(defun name-ion (ion)
  (if (plusp (ion-charge ion))
      (ion-name ion)
      (ion-name-root ion "ide")))

(defun compound-p (x)
  (find x *compounds*))

(defun element-p (x)
  (find x *elements*))

(defun ion-charge (ion)
  (let ((compound-entry (compound-p ion))
	(element-entry (element-p ion)))
    (or (getf compound-entry :charge)
	(getf element-entry :charge)
	(error "Ion not found in element or compound records."))))

(defun read-compounds (formula))

(defun clamp-value (value min-value max-value)
  (max (min value max-value) min-value))

(defun string->vector (string)
  (let ((result (make-array (length string)
			    :element-type 'character
			    :fill-pointer 0)))
    (loop for char across string
       do (vector-push char result))
    result))

(defmacro loop-shrinking-string ((var string) &body body)
  "Loop with a given named string shrinking down until 1 char long."
  (with-gensyms (char)
    `(let ((,var (string->vector ,string)))
       (loop for ,char = nil then (vector-pop-safe ,var nil)
	  while (plusp (length ,var))
	    ,@body))))

(defun find-formula-in (formula list)
  "Return values of the element or symbol and then the length of it."
  (loop-shrinking-string (partial-formula formula)
     with result
     do (setf result (find partial-formula list
			   :test #'string=
			   :key (lambda (x) (getf x :symbol))))
     until result
     finally (return (values result (length partial-formula)))))

(defun read-compound-at (formula &optional (start 0))
  "Read a compound object from a string and start on the number given for `start`. Returns the value of the element or compound found. (Secret values: element, next-index)"
  (let ((formula (subseq formula start)))
    ;; This is how to get `or` to work with the multiple values
    (values-list
     (or
      (multiple-value-bind (compound offset)
	  (find-formula-in formula *compounds*)
	;; If compound is nil, then or will switch move on to searching
	;; the elements.
	(if compound
	    (list compound (+ start (or offset 0))))) ; effectively return
      (multiple-value-bind (element offset)
	  (find-formula-in formula *elements*)
	(list element (+ start (or offset 0))))))))   ; effectively return

(defun read-compounds (formula)
  (let ((length (length formula)))
    (do ((compounds nil)
	 (index 0))
	((= index length) (nreverse compounds))
      (multiple-value-bind (a-compound an-index)
	  (read-compound-at formula index)
	(cond
	  (a-compound
	   (push a-compound compounds)
	   (setf index an-index))
	  (t (incf index)))))))

(defun vector-pop-safe (vector sentinel)
  "Try to pop from a vector and return the sentinel value if there is an error."
  ;; This handler case catches popping from an empty vector.
  (handler-case (vector-pop vector)
    (simple-error () sentinel)))
