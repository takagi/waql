#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.data)

(plan nil)

;;
;; test WAQL-SYMBOL-P function
;;

(diag "WAQL-SYMBOL-P")

(is (waql-symbol-p 'x) t
    "basic case 1")
(is (waql-symbol-p 1) nil
    "basic case 2")
(is (waql-symbol-p '_) nil
    "basic case 3")
(is (waql-symbol-p 't) nil
    "basic case 4")
(is (waql-symbol-p 'nil) nil
    "basic case 5")


;;
;; test PERCENT-SYMBOL-P function
;;

(diag "PERCENT-SYMBOL-P")

(is (percent-symbol-p '%a) t
    "basic case 1")
(is (percent-symbol-p 'foo) nil
    "basic case 2")

(is-error (percent-symbol-p 1) type-error
          "SYMBOL which is not a WAQL symbol")


;;
;; test PERCENT-SYMBOL function
;;

(diag "PERCENT-SYMBOL")

(let ((symbol (percent-symbol 'x 1)))
  (is symbol '%x1
      "basic case 1")
  (is (original-symbol symbol) 'x
      "basic case 2"))

(let ((symbol (percent-symbol '_ 1)))
  (is symbol '%_1
      "basic case 3")
  (is (original-symbol symbol) '_
      "basic case 4"))

(is-error (percent-symbol 1 1) type-error
          "SYMBOL which is not a WAQL symbol")

(is-error (percent-symbol 'x 'foo) type-error
          "COUNT which is not an integer")


;;
;; test ORIGINAL-SYMBOL function
;;

(diag "ORIGINAL-SYMBOL")

(let ((symbol (percent-symbol 'x 1)))
  (is (original-symbol symbol) 'x
      "basic case 1"))

(is (original-symbol 'x) nil
    "basic case 2")


;;
;; test SCOPED-SYMBOL function
;;

(diag "SCOPED-SYMBOL")

(is (scoped-symbol 'x 'foo) 'foo.x
    "basic case 1")
(is (scoped-symbol 'x nil) 'x
    "basic case 2")

(is-error (scoped-symbol 1 'foo) type-error
          "SYMBOL which is not a WAQL symbol")

(is-error (scoped-symbol 'x 1) type-error
          "SCOPE which is not a WAQL symbol")

(is-error (scoped-symbol 'cl-user::x 'y) simple-error
          "SYMBOL and SCOPE which are not in a same package")


;;
;; test SCOPING-SYMBOL function
;;

(diag "SCOPING-SYMBOL")

(let ((*scoping-count* 1))
  (is (scoping-symbol 'x) '%x1
      "basic case"))

(is-error (scoping-symbol 1) type-error
          "SYMBOL which is not a WAQL symbol")


;;
;; test WAQL-BOOLEAN-P function
;;

(diag "WAQL-BOOLEAN-P")

(is (waql-boolean-p t) t
    "basic case 1")
(is (waql-boolean-p nil) t
    "basic case 2")
(is (waql-boolean-p 1) nil
    "basic case 3")


;;
;; test WAQL-INTEGER-P function
;;

(diag "WAQL-INTEGER-P")

(is (waql-integer-p 1) t
    "basic case 1")
(is (waql-integer-p 'foo) nil
    "basic case 2")


;;
;; test WAQL-STRING-P function
;;

(diag "WAQL-STRING-P")

(is (waql-string-p "foo") t
    "basic case 1")
(is (waql-string-p 1) nil
    "basic case 2")


;;
;; test TIME+ function
;;

(diag "TIME+")

(is (time+ (local-time:parse-timestring "2013-1-1T00:00:00") (days 1))
    (local-time:parse-timestring "2013-1-2T00:00:00")
    :test #'local-time:timestamp=
    "basic case")


;;
;; test TIME- function
;;

(diag "TIME-")

(is (time- (local-time:parse-timestring "2013-1-2T00:00:00") (days 1))
    (local-time:parse-timestring "2013-1-1T00:00:00")
    :test #'local-time:timestamp=
    "basic case")


;;
;; test INTERVAL-UNIT-P function
;;

(diag "INTERVAL-UNIT-P")

(is (interval-unit-p :minute) t
    "basic case 1")
(is (interval-unit-p :hour) t
    "basic case 2")
(is (interval-unit-p :day) t
    "basic case 3")
(is (interval-unit-p :week) nil
    "basic case 4")
(is (interval-unit-p :month) t
    "basic case 5")
(is (interval-unit-p :year) t
    "basic case 6")
(is (interval-unit-p :foo) nil
    "basic case 7")


;;
;; test MINUTES function
;;

(diag "MINUTES")

(let ((interval (minutes 1)))
  (is (interval-amount interval) 1
      "basic case 1")
  (is (interval-unit interval) :minute
      "basic case 2"))


;;
;; test HOURS function
;;

(diag "HOURS")

(let ((interval (hours 1)))
  (is (interval-amount interval) 1
      "basic case 1")
  (is (interval-unit interval) :hour
      "basic case 2"))


;;
;; test DAYS function
;;

(diag "DAYS")

(let ((interval (days 1)))
  (is (interval-amount interval) 1
      "basic case 1")
  (is (interval-unit interval) :day
      "basic case 2"))


;;
;; test WEEKS function
;;

(diag "WEEKS")

(let ((interval (weeks 1)))
  (is (interval-amount interval) 7
      "basic case 1")
  (is (interval-unit interval) :day
      "basic case 2"))


;;
;; test MONTHS function
;;

(diag "MONTHS")

(let ((interval (months 1)))
  (is (interval-amount interval) 1
      "basic case 1")
  (is (interval-unit interval) :month
      "basic case 2"))


;;
;; test YEARS function
;;

(diag "YEARS")

(let ((interval (years 1)))
  (is (interval-amount interval) 1
      "basic case 1")
  (is (interval-unit interval) :year
      "basic case 2"))


;;
;; test TUPLE function
;;

(diag "TUPLE")

(let ((tuple (tuple 1 "foo" 'bar)))
  (is (tuple-ref tuple 0) 1
      "basic case 1")
  (is (tuple-ref tuple 1) "foo"
      "basic case 2")
  (is (tuple-ref tuple 2) 'bar
      "basic case 3")
  (is-print (write tuple) "#S(TUPLE 1 \"foo\" BAR)"
            "basic case 4"))


;;
;; test TUPLE-REF function
;;

(diag "TUPLE-REF")

(is (tuple-ref (tuple 1 2 3) 0) 1
    "basic case 1")

(is (tuple-ref (tuple 1 2 3) 1) 2
    "basic case 2")

(is (tuple-ref (tuple 1 2 3) 2) 3
    "basic case 3")

(is-error (tuple-ref 'foo 0) type-error
          "TUPLE which is not a tuple")

(is-error (tuple-ref (tuple 1 2 3) 'foo) type-error
          "I which is not a tuple")

(is-error (tuple-ref (tuple 1 2 3) -1) type-error
          "I which is not a non-negative integer")

(is-error (tuple-ref (tuple 1 2 3) 3) simple-error
          "I which is equal or larger than dimension of TUPLE")


;;
;; test TUPLE-DIM function
;;

(diag "TUPLE-DIM")

(is (tuple-dim (tuple 1 2 3)) 3
    "basic case")

(is-error (tuple-dim 'foo) type-error
          "TUPLE which is not a tuple")


;;
;; test EMPTY-RELATION-INDEX function
;;

(diag "EMPTY-RELATION-INDEX")

(let ((relation-index (empty-relation-index)))
  (is (lookup-relation-index relation-index 1) nil
      "basic case"))


;;
;; test ADD-RELATION-INDEX function
;;

(diag "ADD-RELATION-INDEX")

(let ((relation-index (empty-relation-index)))
  (add-relation-index relation-index 1 (tuple 1 2 3))
  (is (lookup-relation-index relation-index 1) (list (tuple 1 2 3))
      :test #'equalp
      "basic case"))

(is-error (add-relation-index 'foo 1 (tuple 1 2 3)) type-error
          "INDEX which is not a relation index")

(let ((relation-index (empty-relation-index)))
  (is-error (add-relation-index relation-index 1 'foo) type-error
            "TUPLE which is not a tuple"))


;;
;; test LOOKUP-RELATION-INDEX function
;;

(diag "LOOKUP-RELATION-INDEX")

(let ((relation-index (empty-relation-index)))
  (add-relation-index relation-index 1 (tuple 1 2 3))
  (add-relation-index relation-index 1 (tuple 1 3 2))
  (add-relation-index relation-index 2 (tuple 2 1 3))
  (is (lookup-relation-index relation-index 1)
      (list (tuple 1 3 2) (tuple 1 2 3))
      :test #'equalp
      "basic case 1")
  (is (lookup-relation-index relation-index 2)
      (list (tuple 2 1 3))
      :test #'equalp
      "basic case 2"))

(is-error (lookup-relation-index 'foo 1) type-error
          "INDEX which is not a relation index")


;;
;; test EMPTY-RELATION function
;;

(diag "EMPTY-RELATION")

(is (relation->list (empty-relation)) nil
    "basic case")


;;
;; test RELATION->LIST function
;;

(diag "RELATION->LIST")

(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1 2 3) relation)
  (is (relation->list relation) (list (tuple 1 2 3))
      :test #'equalp
      "basic case"))

(is-error (relation->list 'foo) 'type-error
          "RELATION which is not a relation")


;;
;; test RELATION-COUNT function
;;

(diag "RELATION-COUNT")

(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1 2 3) relation)
  (relation-adjoin (tuple 2 3 4) relation)
  (relation-adjoin (tuple 2 3 4) relation)
  (is (relation-count relation) 2
      "basic case"))

(is-error (relation-count 'foo) 'type-error
          "RELATION which is not a relation")


;;
;; test RELATION-EXISTS function
;;

(diag "RELATION-EXISTS")

(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1 2 3) relation)
  (is (relation-exists relation) t
      "basic case"))

(is-error (relation-exists 'foo) 'type-error
          "RELATION which is not a relation")


;;
;; test RELATION-MEMBER function
;;

(diag "RELATION-MEMBER")

(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1 2 3) relation)
  (is (relation-member relation (tuple 1 2 3)) t
      "basic case 1")
  (is (relation-member relation (tuple 2 3 4)) nil
      "basic case 2"))

(is-error (relation-member 'foo (tuple 1 2 3)) type-error
          "RELATION which is not a relation")

(let ((relation (empty-relation)))
  (is-error (relation-member relation 'foo) type-error
            "TUPLE which is not a tuple"))


;;
;; test RELATION-ADJOIN function
;;

(diag "RELATION-ADJOIN")

(let ((relation (empty-relation)))
  ;; adjoin tuple to relation
  (relation-adjoin (tuple 1 2 3) relation)
  (is (relation-member relation (tuple 1 2 3)) t
      "basic case 1")
  ;; adjoin another tuple to relation
  (relation-adjoin (tuple 2 3 4) relation)
  (is (relation-member relation (tuple 1 2 3)) t
      "basic case 2")
  (is (relation-member relation (tuple 2 3 4)) t
      "basic case 3")
  ;; adjoining tuple ever adjoined to relation makes nothing
  (relation-adjoin (tuple 2 3 4) relation)
  (is (relation-member relation (tuple 1 2 3)) t
      "basic case 4")
  (is (relation-member relation (tuple 2 3 4)) t
      "basic case 5")
  (is (relation-count relation) 2
      "basic case 6"))

(is-error (relation-adjoin (tuple 1 2 3) 'foo) type-error
          "RELATION which is not a relation")

(let ((relation (empty-relation)))
  (is-error (relation-adjoin 'foo relation) type-error
            "TUPLE which is not a relation"))

(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1 2 3) relation)
  (is-error (relation-adjoin (tuple "foo") relation) simple-error
            "TO FAIL: TUPLE which has different attributes than ones in RELATION"))


;;
;; test RELATION-ADJOIN-ALL function
;;

(diag "RELATION-ADJOIN-ALL")

(let ((relation (relation-adjoin-all (list (tuple 1 2 3)
                                           (tuple 2 3 4)
                                           (tuple 2 3 4))
                                     (empty-relation))))
  (is (relation-member relation (tuple 1 2 3)) t
      "basic case 1")
  (is (relation-member relation (tuple 2 3 4)) t
      "basic case 2")
  (is (relation-count relation) 2
      "basic case 3"))

(is-error (relation-adjoin-all 'foo (empty-relation)) type-error
          "TUPLE-LIST which is not a list")


;;
;; test RELATION-INDEX-LOOKUP function
;;

(diag "RELATION-INDEX-LOOKUP")

(let ((relation (empty-relation)))
  ;; test applying empty relation
  (is (relation-index-lookup relation '()) nil
      "basic case 1")
  ;; adjoin tuples to relation
  (relation-adjoin (tuple 1 1 1) relation)
  (relation-adjoin (tuple 1 1 2) relation)
  (relation-adjoin (tuple 1 2 3) relation)
  (relation-adjoin (tuple 1 2 3) relation)
  ;; test with no lookup-keys
  (is (relation-index-lookup relation '())
      (list (tuple 1 2 3)
            (tuple 1 1 2)
            (tuple 1 1 1))
      :test #'equalp
      "basic case 2")
  ;; test lookup-key (1 nil nil)
  ;;   order of the result is not assured (compare to above one)
  (is (relation-index-lookup relation '((1 nil nil)))
      (list (tuple 1 2 3)
            (tuple 1 1 2)
            (tuple 1 1 1))
      :test #'equalp
      "basic case 3")
  ;; test lookup-key (nil 1 nil)
  (is (relation-index-lookup relation '((nil 1 nil)))
      (list (tuple 1 1 2)
            (tuple 1 1 1))
      :test #'equalp
      "basic case 4")
  ;; test lookup-key (nil nil 1)
  (is (relation-index-lookup relation '((nil nil 1)))
      (list (tuple 1 1 1))
      :test #'equalp
      "basic case 5")
  ;; test selective lookup-keys returning result using lookup-key with
  ;; least one
  (is (relation-index-lookup relation '((nil 2 nil)
                                        (nil nil 2)))
      (list (tuple 1 2 3))
      :test #'equalp
      "basic case 6"))

(is-error (relation-index-lookup 'foo '((1 nil nil))) type-error
          "RELATION which is not a relation")

(let ((relation (relation-adjoin (tuple 1 2 3)
                  (empty-relation))))
  (is-error (relation-index-lookup relation 'foo) type-error
            "LOOKUP-KEYS which is not a list"))

(let ((relation (relation-adjoin (tuple 1 2 3)
                  (empty-relation))))
  (is-error (relation-index-lookup relation '((1 1 1))) simple-error
            "LOOKUP-KEYS of which any are invalid"))


;;
;; test Extending :ITERATE library for relation
;;

(diag "Extending :ITERATE library for relation")

(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1 2 3) relation)
  (relation-adjoin (tuple 4 5 6) relation)
  (let ((relation2 (iterate:iter (for-tuple (x y z) in-relation relation)
                                 (collect-relation (tuple x y z)))))
    (is (relation-member relation2 (tuple 1 2 3)) t
        "basic case 1")
    (is (relation-member relation2 (tuple 4 5 6)) t
        "basic case 2")
    (is (relation-count relation2) 2
        "basic case 3")))


(finalize)
