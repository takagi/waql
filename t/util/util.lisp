#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.util)

(plan nil)

;;
;; test SINGLE function
;;

(diag "SINGLE")

(is (single 'foo) nil
    "basic case 1")

(is (single '(a)) t
    "basic case 2")

(is (single '(a . b)) nil
    "basic case 3")


;;
;; test GROUP function
;;

(diag "GROUP")

(is (group '(a b c d) 2) '((a b) (c d))
    "basic case 1")

(is (group '(a b c d e f) 3) '((a b c) (d e f))
    "basic case 2")

(is (group '(a b c) 2) '((a b) (c))
    "basic case 3")

(is-error (group 'foo 2) type-error
          "SOURCE which is not a list")

(is-error (group '(a b c d) 'foo) type-error
          "N which is not an integer")

(is-error (group '(a b c d) -1) type-error
          "N which is negative")

(is-error (group '(a b c d) 0) simple-error
          "N which is zero")


;;
;; test MINIMIZE function
;;

(diag "MINIMIZE")

(is (minimize '(2 1 3)) 1
    "basic case 1")

(is (minimize '((2 b) (1 a) (3 c)) :key #'car) '(1 a)
    "basic case 2")

(is (minimize '("b" "a" "c") :test #'string<) "a"
    "basic case 3")

(is-error (minimize 'foo) type-error
          "LIST which is not a list")


;;
;; test FLIP function
;;

(diag "FLIP")

(is (funcall (flip #'<) 2 1) t
    "basic case")


;;
;; test SEMICOLON-TERMINATED
;;

(diag "SEMICOLON-TERMINATED")

(is (semicolon-terminated "foo") "foo;"
    "basic case 1")

(is (semicolon-terminated "foo;") "foo;;"
    "basic case 2")

(is-error (semicolon-terminated 'foo) type-error
          "STRING which is not a string")


;;
;; test SEMICOLON-TERMINATED-P
;;

(diag "SEMICOLON-TERMINATED-P")

(is (semicolon-terminated-p "foo") nil
    "basic case 1")

(is (semicolon-terminated-p "foo;") t
    "basic case 2")

(is-error (semicolon-terminated-p 'foo) type-error
          "STRING which is not a string")


;;
;; test ENSURE-SEMICOLON-TERMINATED
;;

(diag "ENSURE-SEMICOLON-TERMINATED")

(is (multiple-value-list (ensure-semicolon-terminated "foo;"))
    '("foo;" nil)
    "basic case 1")

(is (multiple-value-list (ensure-semicolon-terminated "foo"))
    '("foo;" t)
    "basic case 2")

(is-error (ensure-semicolon-terminated 'foo) type-error
          "STRING which is not a string")


(finalize)
