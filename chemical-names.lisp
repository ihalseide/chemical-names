;;;; chemical-names.lisp

(in-package :com.div0.chemical-names)

;; The first element is nil for robustness reasons
(defparameter *numeric-prefixes*
  '(nil mono di tri tetra penta hexa septa octa nona deca)
  "A few prefixes for elements.")

(defparameter *elements* nil
  "A list of the chemical elements on the periodic table.")

(defparameter *compounds* nil
  "A list of the chemical compounds.")

(defun make-element (atomic-number symbol name name-root
                                   period group is-metal &optional charges diatomic)
  "Create information for an element"
  (list :atomic-number atomic-number
        :symbol symbol
        :name name
        :name-root name-root
        :period period
        :group group
        :is-metal is-metal
        :charges (convert-charges charges)
        :diatomic diatomic))

(defun make-compound (symbol name &optional name-root (charge 0))
  "Create information for a compound"
  (list :symbol symbol
        :name name
        :name-root name-root
        :charge (convert-charges charge)))

(defun as-list (obj)
  "Make sure a given object is a list."
  (if (listp obj)
      obj
      (list obj)))

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
                    (number (butlast name)) ; Number is all chars except last
                    (charge (last name))) ; Charge is last char
               ;; Put the charge in front and parse as integer
               (parse-integer (coerce (append charge number) 'string)))))
    ;; Convert each charge, but charges may be a single element, which
    ;; is why it is put through `as-list`
    (mapcar #'convert-charge (as-list charges))))

(defun read-char-if (predicate &optional (stream *standard-input*))
  "Read a character if it matches the given PREDICATE function from the STREAM."
  (let ((peek (peek-char nil stream)))
    (if (funcall predicate peek)
        (read-char stream))))

(defun read-element (&optional (stream *standard-input*))
  "Read an element (symbol) from a `stream`."
  (let* ((first-char (read-char-if #'upper-case-p stream))
         (second-char (read-char-if #'lower-case-p stream)))
    (intern (coerce (remove-if #'null (list first-char second-char))
                    'string))))

(defun read-element-number (&optional (stream *standard-input*))
  "Get a list of an element and the number following it."
  (list (read-element stream)
        (or (read-integer stream)
            ;; If no number is given, the default is 1
            1)))

(defun read-integer (&optional (stream *standard-input*))
  (parse-integer (coerce (loop for char = (read-char stream nil nil)
                               while (and char (digit-char-p char)) collect char)
                         'string)))

;; TODO: implement
(defun name->formula (name-string)
  "Convert a chemical name to a formula."
  (error "Sorry, not implemented yet."))

(defun element->name (element &key amount add-ending-p)
  "Name an element with a prefix based on its abundance and on knowing where it is in the chemical name. Returns a list of symbols"
  (let ((prefix (when amount (number->prefix amount)))
        ;; When adding an ending, use the name root instead of the full name
        (name (get-compound-attribute 'element element
                                      (if add-ending-p
                                          :name-root
                                          :name)))
        (ending (when add-ending-p 'IDE)))
    ;; Remove any nils from the result
    (remove-if #'null (list prefix name ending))))

(defun known-compound->name (compound)
  "Name a compound, fairly simple."
  (get-compound-attribute 'compound compound :name))

(defun formula->name (formula-string)
  ;; Input formula should be a string
  (unless (stringp formula-string)
    (error "Formula `~S` must be a string." formula))
  (with-input-from-string (in formula-string)
    (let ((formula (read-formula in))
          (elements (read-elements in)))
      (cond
        ((eq 'h (first elements))
         ;; Acid naming because H is first
         (name-acid formula elements))
        (t
         "Sorry, unknown.")))))

(defun name-acid (formula elements)
  "Rules for naming an acidic compound, which is anything that starts with H."
  (let* ((compounds (read-compounds formula))
         (anion (second compounds)))
    (if (get-element-by-symbol (getf anion :symbol))
        ;; Binary Acid
        (format nil "hydro~a acid" (name-compound anion "ic"))
        ;; Ternary Acid
        (format nil "~a acid" (name-compound anion (compound-ending anion))))))

(defun name-metal-nonmetal (formula elements)
  "Name a compound that is has a metal and a nonmetal, like NaCl."
  (let ((ions (read-compounds formula)))
    (if (= 2 (length ions))
        (let ((cation (name-compound (first ions)))
              (anion (name-compound (second ions) "ide")))

          (format nil "~a ~a" cation anion))
        (format nil "~{~a~^ ~}" (map 'list #'name-compound ions)))))

(defun name-nonmetal-nonmetal (formula elements)
  "Name a compound that is full of nonmetals, like CO2 (carbon dioxide)."
  (let ((last (1- (length elements))))
    (flet ((name (element-spec position)
             (let* ((element-list (as-list element-spec))
                    ;; The element symbol is first
                    (element (car element-list))
                    ;; The amount (if present) is second
                    (real-amount (or (cadr element-list)
                                     1))
                    ;; Add an ending to the last element in the elements
                    (add-ending-p (= last position))
                    ;; Remove mono- prefix from first element in the elements
                    (amount (unless (and (= 0 position) (= 1 real-amount))
                              real-amount)))
               ;; Use this general function now, using the amount
               ;; that it needs to know, because a null amount signals that
               ;; it should'nt add a prefix.
               (element->name element
                              :amount amount 
                              :add-ending-p add-ending-p))))
      ;; This next line uses `make-index-list` because it is the only
      ;; way I can think of to keep track of the index and pass it to
      ;; the name function:
      (mapcar #'name elements (make-index-list elements)))))

(defun make-index-list (sequence)
  (loop for a from 0 upto (1- (length sequence))
        collect a))

(defun number->prefix (number)
  "Convert a number to the prefix used for it in chemical naming."
  (when number
    ;; No need to subract 1 from the index because the first element is nil
    ;; NOTE: If the number is bigger than expected, the result is nil also.
    (nth number *numeric-prefixes*)))

(defun vowel-char-p (char)
  "Tell if a character is a vowel."
  (member (char-downcase char)
          (list #\a #\e #\i #\o #\u)))

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

(defun load-chem-data ()
  "This loads all of the chemical data stored in files on disk. This must be loaded before any chemical naming will work."
  (with-open-file (in "elements.txt")
    (setf *elements*
          ;; Use a line function to call `make-element`
          (map-lines (line-function #'make-element) in)))
  (with-open-file (in "compounds.txt")
    (setf *compounds*
          ;; Use a line function to call `make-compound`
          (map-lines (line-function #'make-compound) in))))

(defun newline-char-p (char)
  "Return whether a character represents a newline at all."
  (or (char= #\Newline char)
      (char= #\Linefeed char)
      (char= #\Return char)))

(defun get-element-or-compound (symbol)
  "Get a compound or element named by the SYMBOL, putting compounds first because their names are longer, which is useful for the context this function would be called in because parsing of formulas is greedy."
  (or (get-compound symbol)
      (get-element symbol)))

(defun get-compound-attribute (type symbol attribute)
  "Get an `attribute` of an element or compound, where whether it is an element or compound is specified by `type`. If the requested attribute is `:name-root` and that value is nil, then the `:name` attribute is provided instead."
  (flet ((get-it (attribute)
           (ecase type
             (element (getf (get-element symbol) attribute))
             (compound (getf (get-compound symbol) attribute)))))
    (if (equal attribute :name-root)
        (or (get-it :name-root)
          (get-it :name))
        (get-it attribute))))

(defun get-element (symbol)
  "Find an element with the `symbol`."
  (find symbol *elements*
        :key #'(lambda (element) (getf element :symbol))))

(defun get-compound (symbol)
  "Find an compound with the `symbol`."
  (find symbol *compounds*
        :key #'(lambda (compound) (getf compound :symbol))))

;; Load the resource files that have element data etc...
(load-chem-data)

