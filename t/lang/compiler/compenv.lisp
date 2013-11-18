#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.compenv)

(plan nil)

;;
;; test EMPTY-COMPENV function
;;

(diag "EMPTY-COMPENV")

(let ((compenv (empty-compenv)))
  (is (lookup-compenv 'a compenv) nil
      "basic case"))


;;
;; test ADD-QVAR-COMPENV function
;;

(diag "ADD-QVAR-COMPENV")

(let ((compenv (add-qvar-compenv 'b :int
                 (add-qvar-compenv 'a :int
                   (empty-compenv)))))
  (is (lookup-compenv 'a compenv) '(:qvar :int)
      "basic case 1")
  (is (lookup-compenv 'b compenv) '(:qvar :int)
      "basic case 2"))

(let ((compenv (add-qvar-compenv 'a :bool
                 (add-argvar-compenv 'a :int 1
                   (empty-compenv)))))
  (is (lookup-compenv 'a compenv) '(:qvar :bool)
      "shadowing variables already exist"))

(let ((compenv (empty-compenv)))
  (is-error (add-qvar-compenv 1 :int compenv) type-error
            "VAR which is not a WAQL symbol"))

(let ((compenv (empty-compenv)))
  (is-error (add-qvar-compenv 'a 'foo compenv) type-error
            "TYPE which is not a WAQL type"))

(is-error (add-qvar-compenv 'a :int 'foo) type-error
          "COMPENV which is not a compiling environment")


;;
;; test ADD-ARGVAR-COMPENV function
;;

(diag "ADD-ARGVAR-COMPENV")

(let ((compenv (add-argvar-compenv 'b :int '(let (x 1) x)
                 (add-argvar-compenv 'a :int 1
                   (empty-compenv)))))
  (is (lookup-compenv 'a compenv) '(:argvar :int 1)
      "basic case 1")
  (is (lookup-compenv 'b compenv) '(:argvar :int (let (x 1) x))
      "basic case 2"))

(let ((compenv (add-argvar-compenv 'a :int '(let (x 1) x)
                 (add-argvar-compenv 'a :int 1
                   (empty-compenv)))))
  (is (lookup-compenv 'a compenv) '(:argvar :int (let (x 1) x))
      "shadowing variables already exist"))

(let ((compenv (empty-compenv)))
  (is-error (add-argvar-compenv 1 :int 1 compenv) type-error
            "VAR which is not a WAQL symbol"))

(let ((compenv (empty-compenv)))
  (is-error (add-argvar-compenv 'a 'foo 1 compenv) type-error
            "TYPE which is not a WAQL type"))

(is-error (add-argvar-compenv 'a :int 1 'foo) type-error
          "COMPENV which is not a compiling environment")


;;
;; test ADD-LETVAR-COMPENV function
;;

(diag "ADD-LETVAR-COMPENV")

(let* ((compenv0 (empty-compenv))
       (compenv1 (add-letvar-compenv 'a :int 1 compenv0))
       (compenv (add-letvar-compenv 'b :int '(let (x 1) x) compenv1)))
  (is (lookup-compenv 'a compenv) (list :letvar :int 1 compenv0)
      "basic case 1")
  (is (lookup-compenv 'b compenv) (list :letvar :int '(let (x 1) x)
                                        compenv1)
      "basic case 2"))

(let* ((compenv0 (empty-compenv))
       (compenv1 (add-letvar-compenv 'a :int 1 compenv0))
       (compenv (add-letvar-compenv 'a :int '(let (x 1) x) compenv1)))
  (is (lookup-compenv 'a compenv) (list :letvar :int '(let (x 1) x)
                                        compenv1)
      "shadowing variables already exist"))

(let ((compenv (empty-compenv)))
  (is-error (add-argvar-compenv 1 :int 1 compenv) type-error
            "VAR which is not a WAQL symbol"))

(let ((compenv (empty-compenv)))
  (is-error (add-argvar-compenv 'a 1 1 compenv) type-error
            "TYPE which is not a WAQL type"))

(is-error (add-argvar-compenv 'a :int 1 'foo) type-error
          "COMPENV which is not a compiling environment")


;;
;; test ADD-LETFUN-COMPENV function
;;

(diag "ADD-LETFUN-COMPENV")

(let* ((compenv0 (empty-compenv))
       (compenv1 (add-letfun-compenv 'f '((i :int)) :int 'i compenv0))
       (compenv (add-letfun-compenv 'g '((i :int) (j :int)) :bool '(< i j)
                                    compenv1)))
  (is (lookup-compenv 'f compenv)
      (list :letfun '((i :int)) :int 'i compenv0)
      "basic case 1")
  (is (lookup-compenv 'g compenv)
      (list :letfun '((i :int) (j :int)) :bool '(< i j) compenv1)
      "basic case 2"))

(let* ((compenv0 (empty-compenv))
       (compenv1 (add-letfun-compenv 'f '((i :int)) :int 'i compenv0))
       (compenv (add-letfun-compenv 'f '((i :int) (j :int)) :bool '(< i j)
                                   compenv1)))
  (is (lookup-compenv 'f compenv)
      (list :letfun '((i :int) (j :int)) :bool '(< i j) compenv1)
      "shadowing variables already exist"))

(let ((compenv (empty-compenv)))
  (is-error (add-letfun-compenv 1 '((i :int)) :int 'i compenv) type-error
            "VAR which is not a WAQL symbol"))

(let ((compenv (empty-compenv)))
  (is-error (add-letfun-compenv 'f 'foo :int 'i compenv) type-error
            "ARG-LIST which is not a list")
  (is-error (add-letfun-compenv 'f '(foo) :int 'i compenv) simple-error
            "ARG-LIST which is not a list of argument 2"))

(let ((compenv (empty-compenv)))
  (is-error (add-letfun-compenv 'f '((i :int)) 'foo 'i compenv) type-error
            "RETURN-TYPE which is not a WAQL type"))

(is-error (add-letfun-compenv 'f '((i :int)) :int 'i 'foo) type-error
          "COMPENV which is not a compiling environment")


;;
;; test ADD-QVARS-COMPENV function
;;

(diag "ADD-QVARS-COMPENV")

(let ((compenv (add-qvars-compenv '(x y) '(:int :bool)
                 (empty-compenv))))
  (is (lookup-compenv 'x compenv) (list :qvar :int)
      "basic case 1")
  (is (lookup-compenv 'y compenv) (list :qvar :bool)
      "basic case 2"))

(let ((compenv (empty-compenv)))
  (is-error (add-qvars-compenv '(x y) '(:int) compenv) simple-error
            "lengths of VAR-LIST and TYPE-LIST are different"))


;;
;; test ADD-ARGVARS-COMPENV function
;;

(diag "ADD-ARGVARS-COMPENV")

(let ((compenv (add-argvars-compenv '((i :int) (j :bool)) '(1 2)
                 (empty-compenv))))
  (is (lookup-compenv 'i compenv) (list :argvar :int 1)
      "basic case 1")
  (is (lookup-compenv 'j compenv) (list :argvar :bool 2)
      "basic case 2"))


;;
;; test LOOKUP-COMPENV function
;;

(diag "LOOKUP-COMPENV")

(let ((compenv (add-qvar-compenv 'i :int
                 (empty-compenv))))
  (is (lookup-compenv 'i compenv) '(:qvar :int)
      "basic case 1")
  (is (lookup-compenv 'j compenv) nil
      "basic case 2"))

(let ((compenv (empty-compenv)))
  (is-error (lookup-compenv 1 compenv) type-error
            "VAR which is not a WAQL symbol"))

(is-error (lookup-compenv 'a 'foo) type-error
          "COMPENV which is not a compiling environment")


;;
;; test COMPENV->TYPENV function
;;

(diag "COMPENV->TYPENV")

(let ((compenv (add-letfun-compenv 'd '((i :int)) :int 'i
                 (add-letvar-compenv 'c :int 1
                   (add-argvar-compenv 'b :int 1
                     (add-qvar-compenv 'a :bool
                       (empty-compenv)))))))
  (let ((typenv (compenv->typenv compenv)))
    (is (lookup-typenv 'a typenv) :bool
        "basic case 1")
    (is (lookup-typenv 'b typenv) :int
        "basic case 2")
    (is (lookup-typenv 'c typenv) :int
        "basic case 3")
    (is (lookup-typenv 'd typenv) '(:function (:int) :int)
        "basic case 4")))

(is-error (compenv->typenv 'foo) type-error
          "COMPENV which is not a compiling environment")


(finalize)
