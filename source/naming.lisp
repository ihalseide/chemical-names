;;;; naming.lisp
;;;; For doing the actual chemical naming implementations

(in-package :com.div0.chemical-names)

(defun name->formula (name-string)
  "Convert a chemical name to a formula."
  ;; TODO: implement
  (error "Sorry, not implemented yet."))

(defun element->name (element &key amount add-ending?)
  "Name an element with a prefix based on its abundance and on knowing where it is in the chemical name. Returns a list of symbols"
  (let ((prefix (when amount (number->prefix amount)))
        ;; When adding an ending, use the name root instead of the full name
        (name (get-compound-attribute 'element element
                                      (if add-ending-p
                                          :name-root
                                          :name)))
        (ending (when add-ending? 'IDE)))
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
    (let* ((formula (read-formula in))
          (elements (formula->elements formula)))
      (cond
        ((eq 'h (first elements))
         ;; Acid naming because H is first
         (name-acid formula elements))
        (t
         "Sorry, unknown.")))))

(defun acid->name (formula elements)
  "Rules for naming an acidic compound, which is anything that starts with H."
  (let ((anion (second compounds)))
    (if (get-element-by-symbol (getf anion :symbol))
        ;; Binary Acid
        (format nil "hydro~a acid" (name-compound anion "ic"))
        ;; Ternary Acid
        (format nil "~a acid" (name-compound anion (compound-ending anion))))))

(defun metal-nonmetal->name (formula elements)
  "Name a compound that is has a metal and a nonmetal, like NaCl."
  (let ((ions (read-compounds formula)))
    (if (= 2 (length ions))
        (let ((cation (name-compound (first ions)))
              (anion (name-compound (second ions) "ide")))

          (format nil "~a ~a" cation anion))
        (format nil "~{~a~^ ~}" (map 'list #'name-compound ions)))))

(defun nonmetal-nonmetal->name (formula elements)
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
                    (add-ending? (= last position))
                    ;; Remove mono- prefix from first element in the elements
                    (amount (unless (and (= 0 position) (= 1 real-amount))
                              real-amount)))
               ;; Use this general function now, using the amount
               ;; that it needs to know, because a null amount signals that
               ;; it should'nt add a prefix.
               (element->name element
                              :amount amount 
                              :add-ending? add-ending?))))
      ;; This next line uses `make-index-list` because it is the only
      ;; way I can think of to keep track of the index and pass it to
      ;; the name function:
      (mapcar #'name elements (make-index-list elements)))))

(defun number->prefix (number)
  "Convert a number to the prefix used for it in chemical naming. Invalid numbers result in nil."
  (when number
    ;; No need to subract 1 from the index because the first element is nil
    ;; NOTE: If the number is bigger than expected, the result is nil also.
    (nth number *numeric-prefixes*)))

(defun anion-compound-type (compound)
  "Return the compound ending type (-ite or -ate) for a compound."
  (let* ((name (getf compound :name))
         (last-3 (subseq name (- (length name) 3))))
    (cond
      ((string-equal "ate" last-3) :ate)
      ((string-equal "ite" last-3) :ite)
      (t nil))))

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

(defun normalize-elements (formula)
  "Combine multiple instances of the same element in a formula into one instance."
  ;; TODO: refactor & make recursive
  (let1 result nil
        (dolist (el formula)
          (let ((pos (position (car el) result :key #'car)))
            (if pos
                (setf (elt result pos) (combine-compounds (elt result pos) el))
                (push el result))))
        result))

(defun formula->elements (formula)
  (flet ((apply-compound (seq)
           (if (list? (car seq))
               (let ((elements (car seq))
                     (multiplier (cdr seq)))
                 (mapcar #'(lambda (el) (* el multiplier)) elements))
               seq)))
    (normalize-elements (flatten (mapcar #'apply-compound formula)))))

