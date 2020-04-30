;;;; chemical-names.asd

(asdf:defsystem :com.div0.chemical-names
  :description "Convert between chemical formulas and chemical names."
  :author "Izak Halseide <halseide.izak@gmail.com>"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "macros")
               (:file "chemical-names")))
