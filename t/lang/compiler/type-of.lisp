#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.type-of)

(plan nil)

;;
;; test TYPE-OF-LITERAL function
;;

(diag "TYPE-OF-LITERAL")

(is (type-of-literal 1) :int
    "basic case 1")

(is (type-of-literal "foo") :string
    "basic case 2")

(is (type-of-literal '(time "2013-1-1" "00:00:00")) :time
    "basic case 3")

(is-error (type-of-literal t) simple-error
          "EXPR which is an invalid expression.")


;;
;; test TYPE-OF-VARIABLE-REFERENCE function
;;

(diag "TYPE-OF-VARIABLE-REFERENCE")

(let ((typenv (add-letvar-typenv 'x :int
                (empty-typenv))))
  (is (type-of-variable-reference 'x typenv) :int
      "basic case"))

(is-error (type-of-variable-reference 'x 'foo) type-error
          "TYPENV which is not a type environment")

(let ((typenv (empty-typenv)))
  (is-error (type-of-variable-reference 'x typenv) simple-error
            "EXPR which is an unbound variable"))

(let ((typenv (add-letfun-typenv 'f '(:int) :int
                (empty-typenv))))
  (is-error (type-of-variable-reference 'f typenv) simple-error
            "EXPR which is bound to a function"))


;;
;; test TYPE-OF-LET function
;;

(diag "TYPE-OF-LET")

(let ((typenv (empty-typenv)))
  (is (type-of-let '(let (x 1) x) typenv) :int
      "basic case 1"))

(let ((typenv (empty-typenv)))
  (is (type-of-let '(let (f ((i :int)) i)
                      (f 1)) typenv)
      :int
      "basic case 2"))


;;
;; test TYPE-OF-QUERY function
;;

(diag "TYPE-OF-QUERY")

(let ((typenv (add-letvar-typenv 'r '(:relation :int :int)
                (empty-typenv))))
  (is (type-of-query '(query (x y) (<- (x y) r)) typenv)
      '(:relation :int :int)
      "basic case 1"))

(let ((typenv (add-letvar-typenv 'r '(:relation :int :int)
                (empty-typenv))))
  (is-error (type-of-query '(query (x y) (<- (x y) r)
                                         1) typenv)
            simple-error
            "EXPR whose predicate does not return a value of :bool type"))


;;
;; test TYPE-OF-LISP-FORM function
;;

(diag "TYPE-OF-LISP-FORM")

(is (type-of-lisp-form '(lisp (+ 1 2) :int)) :int
    "basic case")


;;
;; test TYPE-OF-FUNCTION function
;;

(diag "TYPE-OF-FUNCTION")

(let ((typenv (add-letfun-typenv 'f '(:int) :int
                (empty-typenv))))
  (is (type-of-function '(f 1) typenv) :int
      "basic case 1"))

(let ((typenv (empty-typenv)))
  (is (type-of-function '(= 1 1) typenv) :bool
      "basic case 2"))

(is-error (type-of-function '(= 1 1) 'foo) type-error
          "TYPENV which is not a type environment")

(let ((typenv (empty-typenv)))
  (is-error (type-of-function '(f 1) typenv) simple-error
            "EXPR whose operator is undefined"))

(let ((typenv (add-letvar-typenv 'f :int
                (empty-typenv))))
  (is-error (type-of-function '(f 1) typenv) simple-error
            "EXPR whose operator is bound to a variable"))

(let ((typenv (add-letfun-typenv 'f '(:int) :int
                (empty-typenv))))
  (is-error (type-of-function '(f 1 2) typenv) simple-error
            "EXPR of which number of operands is invalid"))

(let ((typenv (add-letfun-typenv 'f '(:int) :int
                (empty-typenv))))
  (is-error (type-of-function '(f "foo") typenv) simple-error
            "EXPR of which types of operands are invalid"))


(finalize)
