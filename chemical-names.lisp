;;;; chemical-names.lisp

(in-package #:com.div0.chemical-names)

(defparameter *elements* nil
  "Elements data that should be read from [elements.txt].")

(defparameter *compounds* nil
  "Compounds data that should be read from [compounds.txt].")

(defparameter *exceptions* nil
  "Naming exceptions data that should be read from [exceptions.txt]")

(defun make-element-def (atomic-number symbol name name-root period group is-metal charges)
  "Create and return the known chemical information about an element."
  (list :atomic-number atomic-number
	:symbol symbol
	:name name
        :name-root name-root
	:period period
	:group group
	:is-metal is-metal
	:charges charges))

(defun make-compound-def (symbol name name-root charge)
  "Create and return the known chemical info about a compound."
  (list :symbol symbol
	:name name
	:name-root name-root
	:charge charge))

(defun make-name-def (formula name)
  "Create and return a mapping between a known formula and name of that formula."
  (list :formula formula
	:name name))

(defun make-compound (symbol count)
  (list :symbol symbol
	:count count))

(defun parse-charge-list (seq)
  "Return a list of charges parsed from a sequence."
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
    (let ((value (parse-integer (read-while #'digit-char-p :stream in)))
	  (sign (read-char in nil 'eof)))
      (if (= value 0) 0
	  (cond ((and (characterp sign) (char= sign #\+)) value)
		((and (characterp sign) (char= sign #\-)) (- value))
		(t (error "Found ~S after charge magnitude instead of a sign." sign)))))))

(defun parse-element-def (string)
  "Parse a line in a elements file."
  (destructuring-bind (atomic-number symbol name name-root period  group metal &rest charges)
      (split-string string)
    (make-element-def (parse-integer atomic-number)
		      symbol
		      name
                      name-root
		      (parse-integer period)
		      (parse-integer group)
		      (string-equal metal "metal")
		      (parse-charge-list charges))))

(defun element-metal-p (element)
  "Tell if an element is a metal."
  (getf element :is-metal))

(defun read-while (test &key (stream *standard-input*) (limit nil))
  "Read characters from a stream as long as the test passes on the characters or the length is within the limit given."
   (with-output-to-string (out)
     (loop for c = (peek-char nil stream nil nil)
	for iterations = 0 then (1+ iterations)
	while (and c
		   (funcall test c)
		   (or (null limit)
		       (< iterations limit)))
	do (write-char (read-char stream) out))))

(defun whitespace-char-p (char)
  "Test if a character is whitespace, which is defined as being a space or a non-graphic character."
  (or (char= #\Space char)
      (not (graphic-char-p char))))

(defun read-word (&optional (stream *standard-input*))
  "Read a word delimited by WHITESPACE-CHAR-P."
  (read-while (complement #'whitespace-char-p) :stream stream))

(defun split-string (string &optional (test #'whitespace-char-p))
  "Split a string on characters that pass the given test."
  (with-input-from-string (stream string)
    (loop for subseq = (read-while (complement test) :stream stream)
       when (plusp (length subseq)) collect subseq
       until (zerop (length (read-while test :stream stream))))))

(defun split-string-on (string &rest split-chars)
  "Split a string on all of the characters given."
  (flet ((do-split (char)
	   (find char split-chars)))
    (split-string string #'do-split)))

(defun is-exception (formula)
  "Find out if a formula matches a certain chemical exception."
  (find formula *exceptions*
	:test #'string=
	:key (lambda (entry) (getf entry :formula))))

(defun name->formula (name)
  (error "Not implemented yet."))

(defun formula->name (formula)
  (cond
    ;; Input formula should be a string
    ((not (stringp formula))
     (error "Formula `~S` must be a string." formula))
    
    ;; Detect naming exceptions from the exceptions.txt file
    ((is-exception formula)
     (getf (is-exception formula) :name))
    
    ;; Single elements or compounds get named their names
    ((= 1 (length (read-compounds formula)))
     (getf (read-compound-at formula 0) :name))

    ;; Now name depending on what is in the formula
    (t
     (with-input-from-string (in formula)
       (let ((first-element (getf (read-element in) :element)))
	 (cond
	   ;; Acid naming if starts with H
	   ((string= "H" (getf first-element :symbol))
	    (name-acid formula))
	   ;; Metal-nonmetal naming if starts with metal
	   ((element-metal-p first-element)
	    (name-metal-nonmetal formula))
	   ;; Nonmetal-nonmetal naming otherwise
	   (t
	    (name-nonmetal-nonmetal formula))))))))

(defun name-metal-nonmetal (formula)
  "Name a compound that is has a metal and a nonmetal, like NaCl."
  (let ((ions (read-compounds formula)))
    (if (= 2 (length ions))
	(let ((cation (name-compound (first ions)))
	      (anion (name-compound (second ions) "ide")))
	  (format nil "~a ~a" cation anion))
	(format nil "~{~a~^ ~}" (map 'list #'name-compound ions)))))

(defun name-nonmetal-nonmetal (formula)
  "Name a compound that is full of nonmetals, like CO2 (carbon dioxide)."
  (let* ((elements (sort (with-input-from-string (in formula) (read-elements in))
			 #'<
			 :key #'(lambda (x) (getf x :count))))
	   (end (1- (length elements))))
      (format nil "~{~a~^ ~}"
	      (loop for pair in elements
		 for first = t then nil
		 for index = 0 then (1+ index)
		 for last = nil then (= end index)
		 collect (prefix-element (getf pair :element)
					 (getf pair :count)
					 :is-first first
					 :is-last last)))))

(defun number->prefix (number)
  "Convert a number to the prefix used for it in chemical naming."
  (ecase number
    (1 "mono")
    (2 "di")
    (3 "tri")
    (4 "tetra")
    (5 "penta")
    (6 "hexa")
    (7 "hepta")
    (8 "octa")
    (9 "nona")
    (10 "deca")))

(defun vowel-char-p (char)
  "Tell if a character is a vowel."
  (or (char-equal #\a char)
      (char-equal #\e char)
      (char-equal #\i char)
      (char-equal #\o char)
      (char-equal #\u char)))

(defun prefix-element (element number &key (is-first nil) (is-last nil))
  "Name an element with a prefix based on its abundance and on knowing where it is in the chemical name."
  ;; Detect the "first element exception"
  (when (and is-first (= 1 number))
    (return-from prefix-element (getf element :name)))
  ;; Otherwise prefix away!
  (let ((name (if is-last (name-compound element "ide") (getf element :name))) ; Last element gets -ide ending
	(prefix (number->prefix number)))
    (if (and (vowel-char-p (elt name 0))
	     (char/= #\i (elt prefix (1- (length prefix)))))
	(concatenate 'string
		     (subseq prefix 0 (1- (length prefix)))
		     name)
	(concatenate 'string prefix name))))
    
(defun compound-ending (compound)
  "Return the compound ending for a compound."
  (ecase (anion-compound-type compound)
    (:ate "ic")
    (:ite "ous")))

(defun anion-compound-type (compound)
  "Return the compound ending type (-ite or -ate) for a compound."
  (let* ((name (getf compound :name))
	(last-3 (subseq name (- (length name) 3))))
    (cond
      ((string-equal "ate" last-3) :ate)
      ((string-equal "ite" last-3) :ite)
      (t nil))))

(defun name-acid (acid-formula)
  "Rules for naming an acidic compound, which is anything that starts with H."
  (let* ((compounds (read-compounds acid-formula))
	 (anion (second compounds)))
    (if (get-element-by-symbol (getf anion :symbol))
	;; Binary Acid
	(format nil "hydro~a acid" (name-compound anion "ic"))
	;; Ternary Acid
	(format nil "~a acid" (name-compound anion (compound-ending anion))))))

(defun map-lines (file func)
  "Return a list of lines in a file with the function applied to each line."
  (with-open-file (in file)
    (loop for line = (read-line in nil)
       while line
       if (char/= #\; (elt line 0))
       collect (funcall func line))))

(defun load-chem-data ()
  "This loads all of the chemical data stored in files on disk. This must be loaded before any chemical naming will work."
  (setf *elements* (map-lines "elements.txt" #'parse-element-def))
  (setf *compounds* (map-lines "compounds.txt" #'parse-compound-def))
  (setf *exceptions* (map-lines "exceptions.txt" #'parse-name-def)))

(defun newline-char-p (char)
  "Return whether a character represents a newline at all."
  (or (char= #\Newline char)
      (char= #\Linefeed char)
      (char= #\Return char)))

(defun parse-name-def (string)
  "Used to read exceptions from exceptions.txt. An exception is formated like [formula] [name]\n."
  (with-input-from-string (in string)
    (let ((formula (read-word in)))
      (read-while #'whitespace-char-p :stream in)
      (let ((name (remove-if #'newline-char-p (read-line in))))
	(make-name-def formula name)))))

(defun compound-root-name (compound)
  (or (getf compound :name-root)
      (getf compound :name)))

(defun name-compound (ion &optional ending)
  (let ((name (getf ion :name)))
    (if ending
	(concatenate 'string (compound-root-name ion) ending)
	name)))

(defun clamp-value (value min-value max-value)
  (max (min value max-value) min-value))

(defun string->vector (string)
  (let ((result (make-array (length string)
			    :element-type 'character
			    :fill-pointer 0)))
    (loop for char across string
       do (vector-push char result))
    result))

(defun find-formula-in (formula list)
  "Return values of the element or symbol and then the length of it."
  (do-shrinking-string (partial-formula formula)
     with result
     do (setf result (find partial-formula list
			   :test #'string=
			   :key (lambda (x) (getf x :symbol))))
     until result
     finally (return (values result (length partial-formula)))))

(defun get-element-by-symbol (symbol)
  (find symbol *elements* :key (lambda (x) (getf x :symbol)) :test #'string=))

(defun read-integer (&optional (stream *standard-input*))
  (read-while #'digit-char-p :stream stream))

(defun read-element (&optional (stream *standard-input*))
  (let ((element (get-element-by-symbol
		  (with-output-to-string (out)
		    (write-string (read-while #'upper-case-p :stream stream :limit 1) out)
		    (write-string (read-while #'lower-case-p :stream stream :limit 1) out))))
	(count (or (parse-integer (read-integer stream) :junk-allowed t)
		   1)))
    (if element (list :element element :count count))))

(defun read-elements (&optional (stream *standard-input*))
  (loop for element = (read-element stream)
     while element
     collecting element))

(defun element-p (string)
  (if (and (stringp string)
	   (<= 1 (length string) 2))
      (get-element-by-symbol string)))

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

;; Automatically load the resource files that have element data etc...
(load-chem-data)
