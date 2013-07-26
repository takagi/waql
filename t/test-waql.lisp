#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-test)

(plan nil)

;;;
;;; test User
;;;

(diag "test User")

;;; test USER constructor
(ok (user 1))

;;; test USER-ID accessor
(is (user-id (user 1)) 1)


;;;
;;; test Action Event
;;;

(diag "test Action Event")

;;; test ACTION-EVENT constructor
(ok (action-event 1))

;;; test ACTION-EVENT-ID accessor
(is (action-event-id (action-event 1)) 1)


;;;
;;; test Action
;;;

(diag "test Action")

;;; test ACTION constructor
(ok (action 1))

;;; test ACTION-ID accessor
(is (action-id (action 1)) 1)


;;;
;;; test Conversion Event
;;;

(diag "test Conversion Event")

;;; tset CONVERSION-EVENT constructor
(ok (conversion-event 1))

;;; test CONVERSION-EVENT-ID accessor
(is (conversion-event-id (conversion-event 1)) 1)


;;;
;;; test Conversion
;;;

(diag "test Conversion")

;;; test CONVERSION constructor
(ok (conversion 1))

;;; test CONVERSION-ID accessor
(is (conversion-id (conversion 1)) 1)


;;;
;;; test Tuple
;;;

(diag "test Tuple")

;;; test TUPLE constructor
(ok (tuple 1 2 3))

;;; test WITH-TUPLE macro
(with-tuple (x y z) (tuple 1 2 3)
  (is x 1)
  (is y 2)
  (is z 3))

;;; test TUPLE-P function
(ok (tuple-p (tuple 1 2 3)))
(ok (null (tuple-p 1)))

;;; test EQUALP function for tuple
(ok (equalp (tuple 1 2 3) (tuple 1 2 3)))
(is (equalp (tuple 1 2 3) (tuple 4 5 6)) nil)
(is (equalp (tuple 1 2 3) (tuple 1 2 3 4)) nil)
(ok (equalp (tuple (user 1)) (tuple (user 1))))


;;;
;;; test Relation
;;;

(diag "test Relation")

;;; test EMPTY-RELATION constructor
(ok (empty-relation))

;;; test RELATION->LIST function
(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1) relation)
  (is (relation->list relation) (list (tuple 1)) :test #'equalp))

;;; test RELATION-MEMBER function

;;; test RELATION-COUNT function

;;; test RELATION-ADJOIN function
(let ((cl-test-more:*default-test-function* #'equalp)
      (relation (empty-relation)))
  ;; adjoin tuple to relation
  (relation-adjoin (tuple (user 1)) relation)
  (ok (relation-member (tuple (user 1)) relation))
  ;; adjoin another tuple to relation
  (relation-adjoin (tuple (user 2)) relation)
  (ok (relation-member (tuple (user 1)) relation))
  (ok (relation-member (tuple (user 2)) relation))
  ;; adjoining tuple ever adjoined to relation makes nothing
  (relation-adjoin (tuple (user 2)) relation)
  (ok (relation-member (tuple (user 1)) relation))
  (ok (relation-member (tuple (user 2)) relation))
  (is (relation-count relation) 2))

;;; test RELATION-ADJOIN-ALL function
(let ((cl-test-more:*default-test-function* #'equalp)
      (relation (relation-adjoin-all (list (tuple (user 1))
                                           (tuple (user 2))
                                           (tuple (user 2)))
                                     (empty-relation))))
  (ok (relation-member (tuple (user 1)) relation))
  (ok (relation-member (tuple (user 2)) relation))
  (is (relation-count relation) 2))

;;; error if try to adjoin other tuples having different attributes
(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1) relation)
  (is-error (relation-adjoin (tuple (user 1)) relation) simple-error))

;;; test extension for :ITERATE library on relation
(let ((relation (relation-adjoin (tuple 4 5 6)
                  (relation-adjoin (tuple 1 2 3)
                    (empty-relation)))))
  (let ((relation2 (iterate:iter (for-tuple (x y z) in-relation relation)
                                 (collect-relation (tuple x y z)))))
    (ok (relation-member (tuple 1 2 3) relation2))
    (ok (relation-member (tuple 4 5 6) relation2))))


;;;
;;; test Querying
;;;

(diag "test Querying")

(defparameter +uf1+
  (relation-adjoin-all (list
                        (tuple (user 1) (action-event 1) (action 1)
                               (conversion-event 1) (conversion 1))
                        (tuple (user 1) (action-event 2) (action 2)
                               (conversion-event 1) (conversion 1))
                        (tuple (user 2) (action-event 3) (action 2)
                               (conversion-event 2) (conversion 2)))
                       (empty-relation)))

;;; test projection
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (query (u ae ac) (<- (u ae ac ce cv) +uf1+))))
  (ok (relation-member (tuple (user 1) (action-event 1) (action 1))
                       result))
  (is (relation-count result) 3))

;;; test selection
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (query (u ae ac ce cv) (<- (u ae ac ce cv) +uf1+)
                                     (= (user-id u) 1))))
  (ok (relation-member (tuple (user 1) (action-event 1) (action 1)
                              (conversion-event 1) (conversion 1))
                       result))
  (ok (null (relation-member (tuple (user 2) (action-event 3) (action 2)
                                    (conversion-event 2) (conversion 2))
                             result)))
  (is (relation-count result) 2))

;;; test Cartesian product
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (query (u1 ae1 ac1 ce1 cv1 u2 ae2 ac2 ce2 cv2)
                     (<- (u1 ae1 ac1 ce1 cv1) +uf1+)
                     (<- (u2 ae2 ac2 ce2 cv2) +uf1+))))
  (ok (relation-member (tuple (user 1) (action-event 1) (action 1)
                              (conversion-event 1) (conversion 1)
                              (user 1) (action-event 1) (action 1)
                              (conversion-event 1) (conversion 1))
                       result))
  (is (relation-count result) 9))

;;; test natural join
(let ((cl-test-more:*default-test-function* #'equalp)
      (r1 (relation-adjoin-all (list (tuple (user 1) (action 1))
                                     (tuple (user 2) (action 2)))
                               (empty-relation)))
      (r2 (relation-adjoin-all (list (tuple (user 1) (conversion 1))
                                     (tuple (user 1) (conversion 2)))
                               (empty-relation))))
  (let ((result (query (u1 ac cv)
                       (<- (u1 ac) r1)
                       (<- (u2 cv) r2)
                       (= (user-id u1) (user-id u2)))))
    (ok (relation-member (tuple (user 1) (action 1) (conversion 1))
                         result))
    (ok (relation-member (tuple (user 1) (action 1) (conversion 2))
                         result))
    (is (relation-count result) 2)))


;;;
;;; test Querying - Compiler
;;;

(diag "test Querying - Compiler")

;;; test QUERY macro for projection
(is-expand (query (u ae ac) (<- (u ae ac ce cv) uf))
           (iterate:iter waql::outermost
             (for-tuple (u ae ac ce cv) in-relation uf)
               (iterate:in waql::outermost
                (collect-relation (tuple u ae ac)))))

;;; test QUERY macro for selection
(is-expand (query (u ae ac) (<- (u ae ac ce cv) uf)
                            (= (user-id u) 1))
           (iterate:iter waql::outermost
             (for-tuple (u ae ac ce cv) in-relation uf)
               (when (= (user-id u) 1)
                 (iterate:in waql::outermost
                   (collect-relation (tuple u ae ac))))))

;;; test QUERY macro for Cartesian product
(is-expand (query (u1 ae1 ac1 ce1 cv1 u2 ae2 ac2 ce2 cv2)
                  (<- (u1 ae1 ac1 ce1 cv1) +uf1+)
                  (<- (u2 ae2 ac2 ce2 cv2) +uf1+))
           (iterate:iter waql::outermost
             (for-tuple (u1 ae1 ac1 ce1 cv1) in-relation +uf1+)
               (iterate:iter (for-tuple (u2 ae2 ac2 ce2 cv2)
                                        in-relation +uf1+)
                 (iterate:in waql::outermost
                   (collect-relation
                     (tuple u1 ae1 ac1 ce1 cv1
                            u2 ae2 ac2 ce2 cv2))))))

;;; test QUERY-QUALS function
(is (waql::query-quals '((<- (a b c) foo)) '(a b c) :outermost t)
    '(iterate:iter waql::outermost
                   (for-tuple (a b c) in-relation foo)
                   (iterate:in waql::outermost
                       (collect-relation (tuple a b c)))))

(is (waql::query-quals '((<- (a b c) foo)
                         (<- (d e f) baz)
                         (some-predicate)) '(a b c d e f) :outermost t)
    '(iterate:iter waql::outermost
       (for-tuple (a b c) in-relation foo)
         (iterate:iter (for-tuple (d e f) in-relation baz)
           (when (some-predicate)
             (iterate:in waql::outermost
               (collect-relation (tuple a b c d e f)))))))

;;; test QUERY-QUAL function
(is (waql::query-qual '(<- (a b c) foo) nil '(a b c) t)
    '(iterate:iter waql::outermost
                   (for-tuple (a b c) in-relation foo)
                   (iterate:in waql::outermost
                       (collect-relation (tuple a b c)))))

;;; test QUERY-EXPS function
(is (waql::query-exps '(a b c))
    '(iterate:in waql::outermost
       (collect-relation (tuple a b c))))
(is-error (waql::query-exps 'a) error)


;;;
;;; test Querying - Compiler - Quantification
;;;

(diag "test Querying - Compiler - Quantification")

;;; test QUANTIFICATION-P function
(ok (waql::quantification-p '(<- (a b c) foo)))
(ok (null (waql::quantification-p '(= 1 1))))

;;; test QUANTIFICATION-VARS function
(is (waql::quantification-vars '(<- (a b c) foo)) '(a b c))
(is-error (waql::quantification-vars '(<- a foo)) simple-error)
(is-error (waql::quantification-vars '(= 1 1)) simple-error)

;;; test QUANTIFICATION-RELATION function
(is (waql::quantification-relation '(<- (a b c) foo)) 'foo)
(is-error (waql::quantification-relation '(= 1 1)) simple-error)

;;; test QUERY-QUANTIFICATION function
(is (waql::query-quantification '(<- (a b c) foo) nil '(a b c) t)
    `(iterate:iter waql::outermost
                   (for-tuple (a b c) in-relation foo)
                   ,(waql::query-quals nil '(a b c))))

(is (waql::query-quantification '(<- (a b c) foo) nil '(a b c) nil)
    `(iterate:iter (for-tuple (a b c) in-relation foo)
                   ,(waql::query-quals nil '(a b c))))

(is-error (waql::query-quantification '(<- a foo) nil '(a b c) t)
          simple-error)


;;;
;;; test Querying - Compiler - Predicate
;;;

(diag "test Querying - Compiler - Predicate")

;;; test QUERY-PREDICATE function

(is (waql::query-predicate '(= (user-id u) 1) nil '(a b c))
    `(when (= (user-id u) 1)
       ,(waql::query-quals nil '(a b c))))

(is (waql::query-predicate 'a nil '(a b c))
    `(when a
       ,(waql::query-quals nil '(a b c))))


(finalize)
