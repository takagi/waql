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
;;; test Evaluating WAQL
;;;

(diag "test Evaluating WAQL")

(defparameter +r1+
  (relation-adjoin-all (list (tuple (user 1) (event 1))
                             (tuple (user 1) (event 2))
                             (tuple (user 2) (event 3)))
                       (empty-relation)))

;;; test projection
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u) (<- (u ev) +r1+)))))
  (ok (relation-member (tuple (user 1)) result))
  (is (relation-count result) 2))

;;; test selection
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u ev) (<- (u ev) +r1+)
                                       (lisp (= (user-id u) 1))))))
  (ok (relation-member (tuple (user 1) (event 1)) result))
  (ok (null (relation-member (tuple (user 2) (event 3)) result)))
  (is (relation-count result) 2))

;;; test Cartesian product
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u1 ev1 u2 ev2) (<- (u1 ev1) +r1+)
                                                (<- (u2 ev2) +r1+)))))
  (ok (relation-member (tuple (user 1) (event 1) (user 2) (event 3))
                       result))
  (is (relation-count result) 9))

;;; test natural join
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql
                (query (u1 ev1 ev2) (<- (u1 ev1) +r1+)
                                    (<- (u2 ev2) +r1+)
                                    (= u1 u2)
                                    (lisp (< (event-id ev1)
                                             (event-id ev2)))))))
  (ok (relation-member (tuple (user 1) (event 1) (event 2)) result))
  (is (relation-count result) 1))

;;; test count aggregation
(let ((result (eval-waql
                (query (u (count (query (ev1) (<- (u ev1) +r1+))))
                       (<- (u ev) +r1+)))))
  (ok (relation-member (tuple (user 1) 2) result))
  (ok (relation-member (tuple (user 2) 1) result))
  (is (relation-count result) 2))


;;;
;;; test Checking reserved symbols
;;;

;;; test CHECK-RESERVED-SYMBOLS function
(ok (waql::check-reserved-symbols '(query (u) (<- (u ev) +r1+))))
(is-error (waql::check-reserved-symbols '(query (%u) (<- (u ev) +r1+)))
          simple-error)


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
                        (<- (%a1 d) r2)
                        (= a %a1))))

;;; test pattern matching in join
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match
        '(query (a b c1) (<- (a b) r1)
                         (<- (a1 c1) (query (a c) (<- (a c) r2))))
        patenv)
      '(query (a b c1) (<- (a b) r1)
                       (<- (a1 c1) (query (a c) (<- (%a1 c) r2)
                                                (= a %a1))))))

;;; test pattern matching in selection
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match
        '(query (a b c) (<- (a b) r1)
                        (= (count (query (a c) (<- (a c) r2)))
                           1))
        patenv)
      '(query (a b c) (<- (a b) r1)
                      (= (count (query (a c) (<- (%a1 c) r2)
                                             (= a %a1)))
                         1))))

;;; test pattern matching in projection
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match
        '(query (a (count (query (a c) (<- (a c) r2))))
                (<- (a b) r1))
        patenv)
      '(query (a (count (query (a c) (<- (%a1 c) r2)
                                     (= a %a1))))
              (<- (a b) r1))))


;;; test count aggregation
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match
        '(query (a (count (query (a c) (<- (a c) r2))))
                (<- (a b) r1))
        patenv)
      '(query (a (count (query (a c) (<- (%a1 c) r2)
                                     (= a %a1))))
              (<- (a b) r1))))

;;; test SOLVE-PATTERN-MATCH-QUERY function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-query '(query (a b c d) (<- (a b c) r1)
                                                         (<- (a d) r2))
                                       patenv)
      '(query (a b c d) (<- (a b c) r1)
                        (<- (%a1 d) r2)
                        (= a %a1))))

;;; test SOLVE-PATTERN-MATCH-QUALS function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-quals
        '((<- (a b c) r1) (<- (a d) r2)) '(a b c d)
        patenv)
      '(((<- (a b c) r1) (<- (%a1 d) r2) (= a %a1)) (a b c d))))

;;; test SOLVE-PATTERN-MATCH-QUAL function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-qual
        '(<- (a b c) r1) '((<- (a d) r2)) '(a b c d)
        patenv)
    '((<- (a b c) r1) ((<- (%a1 d) r2) (= a %a1)) (a b c d))))

;;; test SOLVE-PATTERN-MATCH-QUANTIFICATION function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-quantification
        '(<- (a b c) r1) '((<- (a d) r2)) '(a b c d)
        patenv)
      '((<- (a b c) r1) ((<- (%a1 d) r2) (= a %a1)) (a b c d))))

(let ((patenv (waql::patenv-add 'a (waql::empty-patenv))))
  (is (waql::solve-pattern-match-quantification '(<- (a d) r2) nil '(a b c d)
                                                patenv)
      '((<- (%a1 d) r2) ((= a %a1)) (a b c d))))

;;; test SOLVE-PATTERN-MATCH-LISP-FORM function
(is (waql::solve-pattern-match-lisp-form '(lisp (= (user-id u) 1)))
    '(lisp (= (user-id u) 1)))

;;; test SOLVE-PATTERN-MATCH-FUNCTION function
(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-function '(= u u1) patenv)
      '(= u u1)))

(let ((patenv (waql::empty-patenv)))
  (is (waql::solve-pattern-match-function '(count r) patenv)
      '(count r)))


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
      (is vars '(%a1 %b1 c))
      (is (waql::patenv-lookup 'a patenv1) '(a . 2))
      (is (waql::patenv-lookup 'b patenv1) '(b . 2))
      (is (waql::patenv-lookup 'c patenv1) '(c . 1))
      (is preds '((= a %a1)
                  (= b %b1))))))

;;; test PATTERN-MATCHER-SYMBOL function
(is (waql::pattern-matcher-symbol 'a 1) '%a1)
(is-error (waql::pattern-matcher-symbol 1 1) simple-type-error)
(is-error (waql::pattern-matcher-symbol 'a 'a) simple-type-error)


;;; test PATTERN-MATCHER-MATCH-ALL function
(let ((patenv (waql::patenv-add 'b
                (waql::patenv-add 'a (waql::empty-patenv)))))
  (let ((matcher (waql::pattern-matcher-match-all '(a b c)
                   (waql::make-pattern-matcher patenv))))
    (destructuring-bind (vars patenv1 preds)
        (waql::pattern-matcher-result matcher)
      (is vars '(%a1 %b1 c))
      (is (waql::patenv-lookup 'a patenv1) '(a . 2))
      (is (waql::patenv-lookup 'b patenv1) '(b . 2))
      (is (waql::patenv-lookup 'c patenv1) '(c . 1))
      (is preds '((= a %a1)
                  (= b %b1))))))


;;;
;;; test Compiler
;;;

(diag "test Compiler")

(is (waql::compile-expression
      '(query (a1 c) (<- (a b) r1)
                     (<- (a1 c) (query (a c) (<- (b1 c) r2)
                                             (= b b1)))))
    '(iterate:iter waql::outermost
       (for-tuple (a b) in-relation r1)
         (iterate:iter (for-tuple (a1 c) in-relation
                         (iterate:iter waql::outermost
                           (for-tuple (b1 c) in-relation r2)
                           (when (equalp b b1)
                             (iterate:in waql::outermost
                               (collect-relation (tuple a c))))))
                       (iterate:in waql::outermost
                         (collect-relation (tuple a1 c))))))


;;;
;;; test Compiler - literal
;;;

(diag "test Compiler - literal")

;;; test LITERAL-P function
(ok (waql::literal-p 1))
(ok (null (waql::literal-p 'a)))


;;;
;;; test Compiler - symbol
;;;

(diag "test Compiler - symbol")

;;; test SYMBOL-P function
(ok (waql::symbol-p 'a))
(ok (null (waql::symbol-p 1)))


;;;
;;; test Compiler - query
;;;

(diag "test Compiler - query")

;;; test COMPILE-QUERY function

;;; test COMPILE-QUERY-QUALS function
(is (waql::compile-query-quals '((<- (a b c) foo)) '(a b c) :outermost t)
    '(iterate:iter waql::outermost
       (for-tuple (a b c) in-relation foo)
         (iterate:in waql::outermost
           (collect-relation (tuple a b c)))))

(is (waql::compile-query-quals '((<- (a b c) foo)
                                 (<- (d e f) baz)
                                 (= a d))
                               '(a b c d e f) :outermost t)
    `(iterate:iter waql::outermost
       (for-tuple (a b c) in-relation foo)
         (iterate:iter (for-tuple (d e f) in-relation baz)
           (when (equalp a d)
             (iterate:in waql::outermost
               (collect-relation (tuple a b c d e f)))))))

;;; test COMPILE-QUERY-QUAL function
(is (waql::compile-query-qual '(<- (a b c) foo) nil '(a b c) t)
    '(iterate:iter waql::outermost
       (for-tuple (a b c) in-relation foo)
         (iterate:in waql::outermost
           (collect-relation (tuple a b c)))))

;;; test COMPILE-QUERY-EXPRS function
(is (waql::compile-query-exprs '(a b c))
    '(iterate:in waql::outermost
       (collect-relation (tuple a b c))))

(is-error (waql::compile-query-exprs 'a) type-error)


;;;
;;; test Compiler - query - quantification
;;;

(diag "test Compiler - query - quantification")

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

;;; test COMPILE-QUANTIFICATION function
(is (waql::compile-quantification '(<- (a b c) foo) nil '(a b c) t)
    `(iterate:iter waql::outermost
       (for-tuple (a b c) in-relation foo)
         ,(waql::compile-query-quals nil '(a b c))))

(is (waql::compile-quantification '(<- (a b c) foo) nil '(a b c) nil)
    `(iterate:iter (for-tuple (a b c) in-relation foo)
       ,(waql::compile-query-quals nil '(a b c))))

(is-error (waql::compile-quantification '(<- a foo) nil '(a b c) t)
          simple-error)


;;;
;;; test Compiler - query - predicate
;;;

(diag "test Compiler - query - predicate")

;;; test COMPILE-PREDICATE function
(is (waql::compile-predicate '(= u u1) nil '(a b c))
    `(when (equalp u u1)
       ,(waql::compile-query-quals nil '(a b c))))

(is (waql::compile-predicate 'a nil '(a b c))
    `(when a
       ,(waql::compile-query-quals nil '(a b c))))


;;;
;;; test Compiler - lisp form
;;;

(diag "test Compiler - lisp form")

;;; test COMPILE-LISP-FORM function
(is (waql::compile-lisp-form '(lisp (= (user-id u) 1)))
    '(= (user-id u) 1))


;;;
;;; test Compiler - function application
;;;

(diag "test Compiler - function application")

;;; test COMPILE-FUNCTION function
(is (waql::compile-function '(= u u1))
    '(equalp u u1))

(is (waql::compile-function '(count r))
    '(relation-count r))


(finalize)
