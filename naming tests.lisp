
(in-package :com.div0.chemical-names.test)

(defun check-naming (formula expected-name)
  (string= (name-formula formula) expected-name))

(deftest test-invalid ()
  (check
   (eql (name-formula "") nil)
   (eql (name-formula " ") nil)))

(deftest test-exceptions ()
  (check
    (string= (name-formula "H2O") "water")
    (check-naming "O3" "ozone")
    (check-naming "HCN" "hydrogen cyanide")))

(deftest test-metal-nonmetal ()
  (check
    (string= (name-formula "NaCl") "sodium chloride")
    (string= (name-formula "CaCl2") "calcium chloride")))

(deftest test-nonmetal-nonmetal ()
  (check
    (string= (name-formula "CO2") "carbon dioxide")
    (string= (name-formula "CO") "carbon monoxide")
    (check-naming "NO" "nitrogen monoxide")
    (check-naming "O2" "dioxygen")
    (check-naming "H2O2" "dihydrogen dioxide")
    (check-naming "O" "oxygen")))

(deftest test-acids ()
  (check
    (check-naming "HCl" "hydrochloric acid")))

(defun test ()
  (load-chem-info)
  (format t "~%Starting tests.~%")
  (test-invalid)
  (test-exceptions)
  (test-metal-nonmetal)
  (test-nonmetal-nonmetal)
  (test-acids)
  (format t "~%Done.~%"))
