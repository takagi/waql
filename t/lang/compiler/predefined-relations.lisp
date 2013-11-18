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

(let ((relation (empty-relation)))
  (let ((predefined-relations
          (add-predefined-relation 'x '(:int) relation
            (add-predefined-relation 'x '(:string) relation
              (empty-predefined-relations)))))
    (is predefined-relations
        (list (list 'x '(:relation :int) relation))
        "basic case 1")
    (is (length predefined-relations) 1
        "basic case 2")))

(let ((relation (empty-relation)))
  (is-error (add-predefined-relation 1 '(:int) relation
              (empty-relation))
            type-error
            "VAR which is not a WAQL symbol"))

(let ((relation (empty-relation)))
  (is-error (add-predefined-relation 'x 'foo relation
              (empty-relation))
            type-error
            "ATTRIBUTE-TYPES which is not a list of WAQL types"))

(let ((relation (empty-relation)))
  (is-error (add-predefined-relation 'x '(:int) 'foo
              (empty-relation))
            type-error
            "RELATION which is not a relation"))

(let ((relation (empty-relation)))
  (is-error (add-predefined-relation 'x '(:int) relation 'foo) type-error
            "PREDEFINED-RELATIONS which is not predefined relations"))


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

(destructuring-bind (var type relation)
    (car (predefined-relations))
  (is var 'x
      "basic case 1")
  (is type '(:relation :int)
      "basic case 2")
  (is (relation-count relation) 0
      "basic case 3"))


(finalize)
