#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.predefined-relations)

(plan nil)

;;
;; test EMPTY-PREDEFINED-RELATIONS function
;;

(diag "EMPTY-PREDEFINED-RELATIONS")

(is (empty-predefined-relations) nil
    "basic case")


;;
;; test ADD-PREDEFINED-RELATION function
;;

(diag "ADD-PREDEFINED-RELATION")

(let ((predefined-relations
        (add-predefined-relation 'x '(:int)
          (add-predefined-relation 'x '(:string)
            (empty-predefined-relations)))))
  (is predefined-relations
      (list (cons 'x '(:relation :int)))
      "basic case 1")
  (is (length predefined-relations) 1
      "basic case 2"))

(is-error (add-predefined-relation 1 '(:int)
            (empty-relation))
          type-error
          "VAR which is not a WAQL symbol")

(is-error (add-predefined-relation 'x 'foo
            (empty-relation))
          type-error
          "ATTRIBUTE-TYPES which is not a list of WAQL types")

(is-error (add-predefined-relation 'x '(:int) 'foo) type-error
          "PREDEFINED-RELATIONS which is not predefined relations")


;;
;; test CLEAR-DEFRELATION function
;;

(diag "CLEAR-DEFRELATION")

(defrelation x (:int)
  (empty-relation))

(clear-defrelation)

(is (predefined-relations) nil
    "basic case")


;;
;; test DEFRELATION macro
;;

(diag "DEFRELATION")

(clear-defrelation)

(defrelation x (:int)
  (empty-relation))

(destructuring-bind (var . type)
    (car (predefined-relations))
  (is var 'x
      "basic case 1")
  (is type '(:relation :int)
      "basic case 2"))


(finalize)
