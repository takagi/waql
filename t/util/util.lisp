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


(finalize)
