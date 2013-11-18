#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.typenv)

(plan nil)

;;
;; test EMPTY-TYPENV function
;;

(diag "EMPTY-TYPENV")

(let ((typenv (empty-typenv)))
  (is (lookup-typenv 'a typenv) nil
      "basic case"))


;;
;; test ADD-TYPENV function
;;

(diag "ADD-TYPENV")

(let ((typenv (add-typenv 'b :int
                (add-typenv 'a :int
                  (empty-typenv)))))
  (is (lookup-typenv 'a typenv) :int
      "basic case 1")
  (is (lookup-typenv 'b typenv) :int
      "basic case 2"))

(let ((typenv (add-typenv 'a :bool
                (add-typenv 'a :int
                  (empty-typenv)))))
  (is (lookup-typenv 'a typenv) :bool
      "shadowing variables already exist"))

(let ((typenv (empty-typenv)))
  (is-error (add-typenv 1 :int typenv) type-error
            "VAR which is not a WAQL symbol"))

(let ((typenv (empty-typenv)))
  (is-error (add-typenv 'a :foo typenv) type-error
            "TYPE which is not a WAQL type"))

(is-error (add-typenv 'a :int 'foo) type-error
          "TYPENV which is not a type environment")


;;
;; test ADD-QVAR-TYPENV function
;;

(diag "ADD-QVAR-TYPENV")

(let ((typenv (add-qvar-typenv 'a :int
                (empty-typenv))))
  (is (lookup-typenv 'a typenv) :int
      "basic case"))

(let ((typenv (add-qvar-typenv 'a :int
                (empty-typenv))))
  (is-error (add-qvar-typenv 'a :int typenv) simple-error
            "VAR which already exists in TYPENV"))


;;
;; test ADD-ARGVAR-TYPENV function
;;

(diag "ADD-ARGVAR-TYPENV")

(let ((typenv (add-argvar-typenv 'a :int
                (empty-typenv))))
  (is (lookup-typenv 'a typenv) :int
      "basic case"))


;;
;; test ADD-LETVAR-TYPENV function
;;

(diag "ADD-LETVAR-TYPENV")

(let ((typenv (add-letvar-typenv 'a :int
                (empty-typenv))))
  (is (lookup-typenv 'a typenv) :int
      "basic case"))


;;
;; test ADD-LETFUN-TYPENV function
;;

(diag "ADD-LETFUN-TYPENV")

(let ((typenv (add-letfun-typenv 'f '(:int :int) :int
                (empty-typenv))))
  (is (lookup-typenv 'f typenv) '(:function (:int :int) :int)
      "basic case"))


;;
;; test ADD-QVARS-TYPENV function
;;

(diag "ADD-QVARS-TYPENV")

(let ((typenv (add-qvars-typenv '(x y) '(:relation :int :int)
                (empty-typenv))))
  (is (lookup-typenv 'x typenv) :int
      "basic case 1")
  (is (lookup-typenv 'y typenv) :int
      "basic case 2"))

(let ((typenv (empty-typenv)))
  (is-error (add-qvars-typenv '(x y) :int typenv) simple-error
            "RELATION-TYPE which is not a relation type"))

(let ((typenv (empty-typenv)))
  (is-error (add-qvars-typenv '(x) '(:relation :int :int) typenv)
            simple-error
            "VAR-LIST whose length does not match the dimension of RELATION-TYPE"))


;;
;; test ADD-ARGVARS-TYPENV function
;;

(diag "ADD-ARGVARS-TYPENV")

(let ((typenv (add-argvars-typenv '((i :int) (j :int))
                (empty-typenv))))
  (is (lookup-typenv 'i typenv) :int
      "basic case 1")
  (is (lookup-typenv 'j typenv) :int
      "basic case 2"))


;;
;; test LOOKUP-TYPENV function
;;

(diag "LOOKUP-TYPENV")

(let ((typenv (add-typenv 'a :int
                (empty-typenv))))
  (is (lookup-typenv 'a typenv) :int
      "basic case 1")
  (is (lookup-typenv 'b typenv) nil
      "basic case 2"))

(let ((typenv (empty-typenv)))
  (is-error (lookup-typenv 1 typenv) type-error
            "VAR which is not a WAQL symbol"))

(is-error (lookup-typenv 'a 'foo) type-error
          "TYPENV which is not a type environment")


(finalize)
