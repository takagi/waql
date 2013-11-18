#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.patenv)

(plan nil)

;;
;; test EMPTY-PATENV function
;;

(diag "EMPTY-PATENV")

(let ((patenv (empty-patenv)))
  (is (lookup-patenv 'a patenv) nil
      "basic case"))


;;
;; test ADD-PATENV function
;;

(diag "ADD-PATENV")

(let ((patenv (add-patenv 'b
                (add-patenv 'a
                  (empty-patenv)))))
  (is (lookup-patenv 'a patenv) '(a . 1)
      "basic case 1")
  (is (lookup-patenv 'b patenv) '(b . 1)
      "basic case 2"))

(let ((patenv (empty-patenv)))
  (is-error (add-patenv 1 patenv) type-error
            "VAR of invalid type"))

(is-error (add-patenv 'a 'foo) type-error
          "PATENV of invalid type")

(let ((patenv (add-patenv 'a
                (empty-patenv))))
  (is-error (add-patenv 'a patenv) simple-error
            "VAR which already exists"))


;;
;; test BULK-ADD-PATENV function
;;

(diag "BULK-ADD-PATENV")

(let ((patenv (bulk-add-patenv '(a b)
                (empty-patenv))))
  (is (lookup-patenv 'a patenv) '(a . 1)
      "basic case 1")
  (is (lookup-patenv 'b patenv) '(b . 1)
      "basic case 2"))


;;
;; test INC-PATENV function
;;

(diag "INC-PATENV")

(let ((patenv (add-patenv 'b
                (inc-patenv 'a
                  (add-patenv 'a
                    (empty-patenv))))))
  (is (lookup-patenv 'a patenv) '(a . 2)
      "basic case 1")
  (is (lookup-patenv 'b patenv) '(b . 1)
      "basic case 2"))

(let ((patenv (empty-patenv)))
  (is-error (inc-patenv 1 patenv) type-error
            "VAR of invalid type"))

(is-error (inc-patenv 'a 'foo) type-error
          "PATENV of invalid type")

(let ((patenv (empty-patenv)))
  (is-error (inc-patenv 'a patenv) simple-error
            "VAR which does not exist"))


;;
;; test LOOKUP-PATENV function
;;

(diag "LOOKUP-PATENV")

(let ((patenv (add-patenv 'a
                (empty-patenv))))
  (is (lookup-patenv 'a patenv) '(a . 1)
      "basic case 1")
  (is (lookup-patenv 'b patenv) nil
      "basic case 2"))

(let ((patenv (empty-patenv)))
  (is-error (lookup-patenv 1 patenv) type-error
            "VAR of invalid type"))

(is-error (lookup-patenv 'a 'foo) type-error
          "TYPENV of invalid type")


(finalize)
