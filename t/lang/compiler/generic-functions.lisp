#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.generic-functions)

(plan nil)

;;
;; test GENERIC-FUNCTION-P function
;;

(diag "GENERIC-FUNCTION-P")

(is (generic-function-p '=) t
    "basic case 1")

(is (generic-function-p 'foo) nil
    "basic case 2")

(is-error (generic-function-p 1) type-error
          "SYMBOL which is not a WAQL symbol")


;;
;; test SPECIALIZED-FUNCTION function
;;

(diag "SPECIALIZED-FUNCTION")

(let ((func (specialized-function '= '(:int :int))))
  (is (specialized-function-name func) '=
      "basic case 1")
  (is (specialized-function-return-type func) :bool
      "basic case 2"))

(is-error (specialized-function 'foo '()) simple-error
          "FUNCTION-NAME which does not exist")

(is-error (specialized-function '= '(:int)) simple-error
          "TYPE-LIST to which any entries corresponding does not exist")


;;
;; test MAKE-SPECIALIZED-FUNCTION function
;;

(diag "MAKE-SPECIALIZED-FUNCTION")

(let ((func (make-specialized-function '= '= '(:int :int) :int)))
  (is (specialized-function-name func) '=
      "basic case 1")
  (is (specialized-function-generic-name func) '=
      "basic case 2")
  (is (specialized-function-argument-types func) '(:int :int)
      "basic case 3")
  (is (specialized-function-return-type func) :int
      "basic case 4"))


;;
;; test MATCH-TYPE-PATTERN function
;;

(diag "MATCH-TYPE-PATTERN")

(is (match-type-pattern '((:relation :int :string))
                        '(:relation))
                        
    t
    "basic case")

(is-error (match-type-pattern 'foo '(:relation :int :string)) type-error
          "TYPE-LIST which is not a list")

(is-error (match-type-pattern '((:relation :int) :int :string)
                              'foo)
          type-error
          "PATTERN-LIST which is not a list")

(is-error (match-type-pattern '(:int) '(:int :string)) simple-error
          "TYPE-LIST and PATTERN-LIST whose lengths are different")


(finalize)
