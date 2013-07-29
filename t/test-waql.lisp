#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-test)

(plan nil)


;;;
;;; User
;;;

(defstruct (user (:constructor user (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Event
;;;

(defstruct (event (:constructor event (id)))
  (id nil :type fixnum :read-only t))


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

;;; test PRINT-TUPLE function
(is-print (print-object (tuple 1 2 3) *standard-output*)
          "#S(TUPLE 1 2 3)")

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

;;; test PRINT-RELATION function
(is-print (print-object (relation-adjoin (tuple 4 5 6)
                          (relation-adjoin (tuple 1 2 3)
                            (empty-relation)))
                        *standard-output*)
          "#S(RELATION 2 #S(TUPLE 4 5 6) #S(TUPLE 1 2 3))")

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
;;; test Solving pattern match
;;;
;;; original: (query (a b c d) (<- (a b c) r1)
;;;                            (<- (a d) r2))
;;;
;;; solved  : (query (a b c d) (<- (a b c) r1)
;;;                            (<- (a1 d) r2)
;;;                            (= a a1))
;;;

(diag "test Solving pattern match")

;;; test SOLVE-PATTERN-MATCH function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match '(query (a b c d) (<- (a b c) r1)
                                                   (<- (a d) r2))
                                 patenv)
      '(query (a b c d) (<- (a b c) r1)
                        (<- (a1 d) r2)
                        (= a a1))))

;;; test SOLVE-PATTERN-MATCH-QUERY function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-query '(query (a b c d) (<- (a b c) r1)
                                                         (<- (a d) r2))
                                       patenv)
      '(query (a b c d) (<- (a b c) r1)
                        (<- (a1 d) r2)
                        (= a a1))))

;;; test SOLVE-PATTERN-MATCH-QUALS function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-quals
        '((<- (a b c) r1) (<- (a d) r2)) '(a b c d)
        patenv)
      '(((<- (a b c) r1) (<- (a1 d) r2) (= a a1)) (a b c d))))

;;; test SOLVE-PATTERN-MATCH-QUAL function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-qual
        '(<- (a b c) r1) '((<- (a d) r2)) '(a b c d)
        patenv)
    '((<- (a b c) r1) ((<- (a1 d) r2) (= a a1)) (a b c d))))

;;; test SOLVE-PATTERN-MATCH-QUANTIFICATION function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-quantification
        '(<- (a b c) r1) '((<- (a d) r2)) '(a b c d)
        patenv)
      '((<- (a b c) r1) ((<- (a1 d) r2) (= a a1)) (a b c d))))

(let ((patenv (waql::patenv-add 'a (waql::empty-patenv))))
  (is (waql::solve-pattern-match-quantification '(<- (a d) r2) nil '(a b c d)
                                                patenv)
      '((<- (a1 d) r2) ((= a a1)) (a b c d))))


;;;
;;; test Pattern matching environment
;;;

(diag "test Pattern matching environment")

;;; test EMPTY-PATENV constructor and PATENV-LOOKUP function
(ok (null (waql::patenv-lookup 'a (waql::empty-patenv))))

;;; test PATENV-ADD function
(let ((patenv (waql::patenv-add 'b
                (waql::patenv-add 'a (waql::empty-patenv)))))
  (is (waql::patenv-lookup 'a patenv) '(a . 1))
  (is (waql::patenv-lookup 'b patenv) '(b . 1)))

;;; error if trying to add duplicated symbol
(let ((patenv (waql::patenv-add 'a (waql::empty-patenv))))
  (is-error (waql::patenv-add 'a patenv) simple-error))

;;; test PATENV-INC function
(let ((patenv (waql::patenv-inc 'a
                (waql::patenv-add 'b
                  (waql::patenv-add 'a (waql::empty-patenv))))))
  (is (waql::patenv-lookup 'a patenv) '(a . 2)))

;;; error if trying to increment non-exist symbol
(let ((patenv (waql::patenv-add 'a (waql::empty-patenv))))
  (is-error (waql::patenv-inc 'b patenv) simple-error))

;;; test PRINT-PATENV function
(let ((patenv (waql::patenv-inc 'a
                (waql::patenv-add 'b
                  (waql::patenv-add 'a (waql::empty-patenv))))))
  (is-print (print-object patenv *standard-output*)
            "#S(WAQL::PATENV (B . 1) (A . 2))"))


;;;
;;; test Pattern matcher
;;;

(diag "test Pattern matcher")

;;; test MAKE-PATTERN-MATCHER constructor and PATTERN-MATCHER-RESULT selector
(let ((patenv (waql::empty-patenv)))
  (let ((matcher (waql::make-pattern-matcher patenv)))
    (destructuring-bind (vars patenv1 preds)
        (waql::pattern-matcher-result matcher)
      (is vars nil)
      (is (waql::patenv-lookup 'a patenv1) nil)
      (is preds nil))))

;;; test PATTERN-MATCHER-MATCH function
(let ((patenv (waql::patenv-add 'b
                (waql::patenv-add 'a (waql::empty-patenv)))))
  (let ((matcher (waql::pattern-matcher-match 'c
                   (waql::pattern-matcher-match 'b
                     (waql::pattern-matcher-match 'a
                       (waql::make-pattern-matcher patenv))))))
    (destructuring-bind (vars patenv1 preds)
        (waql::pattern-matcher-result matcher)
      (is vars '(a1 b1 c))
      (is (waql::patenv-lookup 'a patenv1) '(a . 2))
      (is (waql::patenv-lookup 'b patenv1) '(b . 2))
      (is (waql::patenv-lookup 'c patenv1) '(c . 1))
      (is preds '((= a a1)
                  (= b b1))))))

;;; test PATTERN-MATCHER-MATCH-ALL function
(let ((patenv (waql::patenv-add 'b
                (waql::patenv-add 'a (waql::empty-patenv)))))
  (let ((matcher (waql::pattern-matcher-match-all '(a b c)
                   (waql::make-pattern-matcher patenv))))
    (destructuring-bind (vars patenv1 preds)
        (waql::pattern-matcher-result matcher)
      (is vars '(a1 b1 c))
      (is (waql::patenv-lookup 'a patenv1) '(a . 2))
      (is (waql::patenv-lookup 'b patenv1) '(b . 2))
      (is (waql::patenv-lookup 'c patenv1) '(c . 1))
      (is preds '((= a a1)
                  (= b b1))))))

;;;
;;; test Querying
;;;

(diag "test Querying")

(defparameter +r1+ (relation-adjoin-all (list (tuple (user 1) (event 1))
                                              (tuple (user 1) (event 2))
                                              (tuple (user 2) (event 3)))
                                        (empty-relation)))

;;; test projection
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (query (u) (<- (u ev) +r1+))))
  (ok (relation-member (tuple (user 1)) result))
  (is (relation-count result) 2))

;;; test selection
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (query (u ev) (<- (u ev) +r1+)
                            (= (user-id u) 1))))
  (ok (relation-member (tuple (user 1) (event 1)) result))
  (ok (null (relation-member (tuple (user 2) (event 3)) result)))
  (is (relation-count result) 2))

;;; test Cartesian product
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (query (u1 ev1 u2 ev2) (<- (u1 ev1) +r1+)
                                     (<- (u2 ev2) +r1+))))
  (ok (relation-member (tuple (user 1) (event 1) (user 2) (event 3))
                       result))
  (is (relation-count result) 9))

;;; test natural join
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (query (u1 ev1 ev2) (<- (u1 ev1) +r1+)
                                  (<- (u2 ev2) +r1+)
                                  (= (user-id u1) (user-id u2))
                                  (< (event-id ev1) (event-id ev2)))))
  (ok (relation-member (tuple (user 1) (event 1) (event 2)) result))
  (is (relation-count result) 1))


;;;
;;; test Querying - Compiler
;;;

(diag "test Querying - Compiler")

;;; test QUERY macro for projection
(is-expand (query (u) (<- (u ev) +r1+))
           (iterate:iter waql::outermost
             (for-tuple (u ev) in-relation +r1+)
               (iterate:in waql::outermost
                 (collect-relation (tuple u)))))

;;; test QUERY macro for selection
(is-expand (query (u ev) (<- (u ev) +r1+)
                         (= (user-id u) 1))
           (iterate:iter waql::outermost
             (for-tuple (u ev) in-relation +r1+)
               (when (= (user-id u) 1)
                 (iterate:in waql::outermost
                   (collect-relation (tuple u ev))))))

;;; test QUERY macro for Cartesian product
(is-expand (query (u1 ev1 u2 ev2) (<- (u1 ev1) +r1+)
                                  (<- (u2 ev2) +r1+))
           (iterate:iter waql::outermost
             (for-tuple (u1 ev1) in-relation +r1+)
               (iterate:iter (for-tuple (u2 ev2) in-relation +r1+)
                 (iterate:in waql::outermost
                   (collect-relation
                     (tuple u1 ev1 u2 ev2))))))

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

;;; test MAKE-QUANTIFICATION constructor
(let ((q (waql::make-quantification '(a b c) 'foo)))
  (is (waql::quantification-vars q) '(a b c))
  (is (waql::quantification-relation q) 'foo))

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
