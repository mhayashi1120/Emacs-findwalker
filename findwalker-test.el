(require 'ert)

(defun findwalker-test-read-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (findwalker--read)))

(defun findwalker-test-compile-string (string &optional inhibit-partial)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (findwalker-edit--args inhibit-partial)))

(ert-deftest findwalker-compile ()
  :tags '(findwalker)
  (should (equal (findwalker-test-compile-string "(name *.el)") '("-name" "\\*.el")))
  (should (equal (findwalker-test-compile-string " (name *.el)") '("-name" "\\*.el")))
  (should (equal (findwalker-test-compile-string "(name *.el) ") '("-name" "\\*.el")))
  (should (equal (findwalker-test-compile-string "(name \"*.el\") ") '("-name" "\\*.el")))
  (should (equal (findwalker-test-compile-string "(name   \"*.el\") ") '("-name" "\\*.el")))
  )

(ert-deftest findwalker-read ()
  :tags '(findwalker)
  (should (equal
           (findwalker-test-read-string "(1 2 \"a\" b ?\\xff ?\\o777 (z y))")
           '("1" "2" "a" "b" "255" "511" ("z" "y"))))
  ;; Not intentional but no need to concern.
  (should (equal (findwalker-test-read-string "(?\\xffzz)") '("255" "zz")))
  (should (equal
           (findwalker-test-read-string "a ")
           "a"))
  (should (equal
           (findwalker-test-read-string " a ")
           "a")))

