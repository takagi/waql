#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.type)

(plan nil)

;;
;; test WAQL-TYPE-P function
;;

(diag "WAQL-TYPE-P")

(is (waql-type-p :bool) t
    "basic case 1")

(is (waql-type-p :int) t
    "basic case 2")

(is (waql-type-p :string) t
    "basic case 3")

(is (waql-type-p :time) t
    "basic case 4")

(is (waql-type-p :interval) t
    "basic case 5")

(is (waql-type-p '(:relation :int)) t
    "basic case 6")

(is (waql-type-p '(:function (:int :int) :int)) t
    "basic case 7")

(is (waql-type-p :foo) nil
    "basic case 8")


;;
;; test MAKE-RELATION-TYPE function
;;

(diag "MAKE-RELATION-TYPE")

(let ((type (make-relation-type '(:int :int))))
  (is (relation-type-attributes type) '(:int :int)
      "basic case"))

(is-error (make-relation-type '(:foo)) type-error
          "ATTR-LIST which is not a list of WAQL types 1")

(is-error (make-relation-type :foo) type-error
          "ATTR-LIST which is not a list of WAQL types 2")


;;
;; test RELATION-TYPE-P function
;;

(diag "RELATION-TYPE-P")

(is (relation-type-p '(:relation :int :int)) t
    "basic case 1")

(is (relation-type-p '(:relation)) t
    "basic case 2")

(is (relation-type-p :foo) nil
    "basic case 3")


;;
;; test RELATION-TYPE-ATTRIBUTES function
;;

(diag "RELATION-TYPE-ATTRIBUTES")

(is (relation-type-attributes '(:relation :int :int)) '(:int :int)
    "basic case 1")

(is (relation-type-attributes '(:relation)) nil
    "basic case 2")

(is-error (relation-type-attributes :foo) simple-error
          "TYPE which is not a WAQL type of relation")


;;
;; test RELATION-TYPE-DIMENSION function
;;

(diag "RELATION-TYPE-DIMENSION")

(is (relation-type-dimension '(:relation :int :int)) 2
    "basic case 1")

(is (relation-type-dimension '(:relation)) 0
    "basic case 2")


;;
;; test MAKE-FUNCTION-TYPE function
;;

(diag "MAKE-FUNCTION-TYPE")

(let ((type (make-function-type '(:int :int) :int)))
  (is (function-type-argument-types type) '(:int :int)
      "basic case 1")
  (is (function-type-return-type type) :int
      "basic case 2"))

(is-error (make-function-type '(:foo) :int) type-error
          "ARG-TYPES which is not a list of WAQL types 1")

(is-error (make-function-type :foo :int) type-error
          "ARG-TYPES which is not a list of WAQL types 2")

(is-error (make-function-type '(:int :int) :foo) type-error
          "RETURN-TYPE which is not a WAQL type")


;;
;; test FUNCTION-TYPE-P function
;;

(diag "FUNCTION-TYPE-P")

(is (function-type-p '(:function (:int :int) :int)) t
    "basic case 1")

(is (function-type-p '(:function)) t
    "basic case 2")

(is (function-type-p :foo) nil
    "basic case 3")


;;
;; test FUNCTION-TYPE-ARGUMENT-TYPES function
;;

(diag "FUNCTION-TYPE-ARGUMENT-TYPES")

(is (function-type-argument-types '(:function (:int :int) :int))
    '(:int :int)
    "basic case")

(is-error (function-type-argument-types '(:function)) simple-error
          "TYPE which is malformed")

(is-error (function-type-argument-types 'foo) simple-error
          "TYPE which is not a WAQL type of function")


;;
;; test FUNCTION-TYPE-RETURN-TYPE function
;;

(diag "FUNCTION-TYPE-RETURN-TYPE")

(is (function-type-return-type '(:function (:int :int) :int)) :int
    "basic case")

(is-error (function-type-return-type '(:function)) simple-error
          "TYPE which is malformed")

(is-error (function-type-return-type 'foo) simple-error
          "TYPE which is not a WAQL type of function")


(finalize)
