;;;; input.lisp

(in-package :com.div0.chemical-names)

(defun read-char-if (predicate &optional (in *standard-input*))
  "Read a character if it matches the given predicate function from the in stream."
  (flet ((matches (char)
           (and char (funcall predicate char))))
    (if (matches (peek-char nil in nil nil))
        (read-char in))))

(defun read-element (&optional (in *standard-input*) read-number?)
  "Read an element (symbol) from a `stream`. and optionally the number after it"
  (let ((*read-eval* nil))                                                         ; No eval for when the element is read in
    (let* ((char1 (read-char-if #'upper-case-p in))
           (char2 (read-char-if #'lower-case-p in)))
      (when char1
        (let ((element (read (make-string-input-stream
                               (coerce (remove-if #'null (list char1 char2))       ; Remove char2 if null
                                       'string)))))
          ;; If the read-number? flag is set, return a list of the element along with the number
          ;; Otherwise, return just the element
          (if read-number?
              (cons element (or (read-int in) 1))
              element))))))

(defun read-int (&optional (in *standard-input*))
  "Read an integer (base 10) from a stream."
  (parse-integer (coerce
                   (loop for char = (read-char-if #'digit-char-p in)
                         while char
                         collect char)
                   'string)
                 :junk-allowed t)) 

(defun read-compound (&optional (in *standard-input*) (read-number? t))
  "Read a single compound or element."
  (if (let ((c (peek-char nil in nil nil))) (and c (char= #\( c)))
      ;; Then: Reading the start of a compound grouping
      (progn
        ;; Discard the opening paren
        (read-char in nil nil)
        ;; This calls read-element instead of calling itself because there won't be nested compounds
        (let ((compound (loop for e = (read-element in t)
                while e collect e)))
          ;; Discard the closing paren
          (read-char in)
          ;; Read the number if wanted
          (if read-number?
              (cons compound (or (read-int in) 1))
              compound)))
      ;; Else: Reading a single element
      (progn
        (read-element in read-number?))))

(defun read-formula (&optional (in *standard-input*))
  "Read all of the compounds in a formula."
  (loop for comp = (read-compound in)
        while comp
        collect comp))

(defun combine-compounds (comp1 comp2)
  "Example: (H . 2) + (H . 3) = (H . 5). Must be the same element or compound."
  (cons (car comp1) ; Element or compound name/representation
        ;; Counts of each one
        (+ (cdr comp1)
           (cdr comp2)))) 
