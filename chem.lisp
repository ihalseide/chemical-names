(in-package #:com.div0.chemical-names)

(defparameter *elements* nil
  "List of all of the elements")

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
    (setf *elements*
	  (loop for line = (read-line stream nil)
	     while line
	     if (char/= (elt line 0) #\;)
	     collect (parse-element-def line)))))

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
