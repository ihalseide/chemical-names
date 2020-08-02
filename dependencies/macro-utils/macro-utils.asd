;;;; macro-utils.asd

(asdf:defsystem :macro-utils
  :description "Some of Izak's macro utilities."
  :author "Izak Halseide <halseide.izak@gmail.com>"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "macro-utils" :depends-on ("packages"))))
