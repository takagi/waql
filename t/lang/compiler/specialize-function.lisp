#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.specialize-function)

(plan nil)

;;
;; test SPECIALIZE-FUNCTION-LITERAL function
;;

(diag "SPECIALIZE-FUNCTION-LITERAL")

(is (specialize-function-literal 1) 1
    "basic case 1")

(is (specialize-function-literal "foo") "foo"
    "basic case 2")

(is (specialize-function-literal '(time "2013-1-1" "00:00:00"))
    '(time "2013-1-1" "00:00:00")
    "basic case 3")


;;
;; test SPECIALIZE-FUNCTION-VARIABLE-REFERENCE function
;;

(diag "SPECIALIZE-FUNCTION-VARIABLE-REFERENCE")

(is (specialize-function-variable-reference 'x) 'x
    "basic case")


;;
;; test SPECIALIZE-FUNCTION-LET function
;;

(diag "SPECIALIZE-FUNCTION-LET")

(let ((typenv (empty-typenv)))
  (is (specialize-function-let '(let (x "foo") (= x "bar")) typenv)
      '(let (x "foo") (string= x "bar"))
      "basic case 1"))

(let ((typenv (empty-typenv)))
  (is (specialize-function-let '(let (f ((i :string)) i)
                                  (= (f "foo") "bar")) typenv)
      '(let (f ((i :string)) i)
         (string= (f "foo") "bar"))
      "basic case 2"))


;;
;; test SPECIALIZE-FUNCTION-QUERY function
;;

(diag "SPECIALIZE-FUNCTION-QUERY")

(let ((typenv (add-letvar-typenv 'r '(:relation :string :string)
                (empty-typenv))))
  (is (specialize-function-query '(query (x y) (<- (x y) r)
                                               (= x y))
                                 typenv)
      '(query (x y) (<- (x y) r)
                    (string= x y))
      "basic case"))


;;
;; test SPECIALIZE-FUNCTION-LISP-FORM function
;;

(diag "SPECIALIZE-FUNCTION-LISP-FORM")

(is (specialize-function-lisp-form '(lisp foo :int))
    '(lisp foo :int)
    "basic case")


;;
;; test SPECIALIZE-FUNCTION-FUNCTION function
;;

(diag "SPECIALIZE-FUNCTION-FUNCTION")

(let ((typenv (add-letfun-typenv 'f '(:int) :int
                (empty-typenv))))
  (is (specialize-function-function '(f 1) typenv) '(f 1)
      "basic case 1"))

(let ((typenv (empty-typenv)))
  (is (specialize-function-function '(= "foo" "bar") typenv)
      '(string= "foo" "bar")
      "basic case 2"))



(finalize)
