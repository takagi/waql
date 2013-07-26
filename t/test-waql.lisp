#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-test)

(plan nil)

;;;
;;; test User
;;;

;;; test USER constructor
(ok (user 1))

;;; test USER-ID accessor
(is (user-id (user 1)) 1)


;;;
;;; test Action Event
;;;

;;; test ACTION-EVENT constructor
(ok (action-event 1))

;;; test ACTION-EVENT-ID accessor
(is (action-event-id (action-event 1)) 1)


;;;
;;; test Action
;;;

;;; test ACTION constructor
(ok (action 1))

;;; test ACTION-ID accessor
(is (action-id (action 1)) 1)


;;;
;;; test Conversion Event
;;;

;;; tset CONVERSION-EVENT constructor
(ok (conversion-event 1))

;;; test CONVERSION-EVENT-ID accessor
(is (conversion-event-id (conversion-event 1)) 1)


;;;
;;; test Conversion
;;;

;;; test CONVERSION constructor
(ok (conversion 1))

;;; test CONVERSION-ID accessor
(is (conversion-id (conversion 1)) 1)


;;;
;;; test Tuple
;;;

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
;;; test Querying - Compiler
;;;

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

;;; test QUERY macro
(is-expand (query (u ae ac) (<- (u ae ac ce cv) uf))
           (iterate:iter (for-tuple (u ae ac ce cv) in-relation uf)
                         (collect-relation (tuple u ae ac))))


;;;
;;; test Querying
;;;

(defparameter +uf1+
  (relation-adjoin-all (list
                        (tuple (user 1) (action-event 1) (action 1)
                               (conversion-event 1) (conversion 1))
                        (tuple (user 1) (action-event 2) (action 2)
                               (conversion-event 1) (conversion 1))
                        (tuple (user 2) (action-event 3) (action 2)
                               (conversion-event 2) (conversion 2)))
                       (empty-relation)))

(let ((cl-test-more:*default-test-function* #'equalp)
      (expected (tuple (user 1) (action-event 1) (action 1))))
  (ok (relation-member expected
                       (query (u ae ac) (<- (u ae ac ce cv) +uf1+)))))


(finalize)
