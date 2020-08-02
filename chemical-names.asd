;;;; chemical-names.asd

(asdf:defsystem :chemical-names
  :description "Convert between chemical formulas and chemical names."
  :author "Izak Halseide"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  ;:depends-on ("macro-utils")
  :components ((:file "packages")
               (:file "data" :depends-on ("packages" "predicates" "macros"))
               (:file "input" :depends-on ("packages" "predicates" "macros"))
               (:file "naming" :depends-on ("packages" "predicates" "macros"))
               (:file "predicates" :depends-on ("packages"))
               (:file "utils" :depends-on ("packages" "predicates" "macros"))
               (:file "macros" :depends-on ("packages"))))
