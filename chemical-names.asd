;;;; chemical-names.asd

(asdf:defsystem "chemical-names"
  :description "Convert between chemical formulas and chemical names."
  :author "Izak Halseide"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :depends-on ("macro-utils" "unit-test")
  :components ((:file "packages")
               (:file "macros" :depends-on ("packages"))
               (:file "chemical-names" :depends-on ("packages" "macros"))))
