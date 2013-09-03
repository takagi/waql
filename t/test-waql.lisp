#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-test)

(plan nil)


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

;;; test TUPLE-DIM function
(is (waql::tuple-dim (tuple 1 2 3)) 3)
(is-error (waql::tuple-dim nil) type-error)

;;; test TUPLE-REF function
(is (waql::tuple-ref (tuple 1 2 3) 0) 1)
(is (waql::tuple-ref (tuple 1 2 3) 1) 2)
(is (waql::tuple-ref (tuple 1 2 3) 2) 3)
(is-error (waql::tuple-ref (tuple 1 2 3) 3) simple-error)
(is-error (waql::tuple-ref (tuple 1 2 3) -1) simple-error)


;;;
;;; test Relation
;;;

(diag "test Relation index")

(let ((index (waql::make-relation-index)))
  (waql::add-relation-index index 'a 1)
  (waql::add-relation-index index 'a 2)
  (waql::add-relation-index index 'b 1)
  (is (waql::lookup-relation-index index 'a) '(2 1))
  (is (waql::lookup-relation-index index 'b) '(1))
  (is (waql::lookup-relation-index index 'c) nil))


(diag "test Relation")

;;; test EMPTY-RELATION constructor
(is (relation->list (empty-relation)) nil)

;;; test RELATION->LIST function
(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1) relation)
  (is (relation->list relation) (list (tuple 1)) :test #'equalp))

;;; test RELATION-MEMBER function
(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1) relation)
  (is (relation-member (tuple 1) relation ) t))

;;; test RELATION-COUNT function
(let ((relation (empty-relation)))
  (relation-adjoin (tuple 1) relation)
  (relation-adjoin (tuple 2) relation)
  (relation-adjoin (tuple 2) relation)
  (is (relation-count relation) 2))

;;; test RELATION-EXISTS function

(let ((relation (relation-adjoin (tuple 1)
                  (empty-relation))))
  (is (relation-exists relation) t))

(is (relation-exists (empty-relation)) nil)

;;; test PRINT-RELATION function
(is-print (print-object (relation-adjoin (tuple 4 5 6)
                          (relation-adjoin (tuple 1 2 3)
                            (empty-relation)))
                        *standard-output*)
          "#S(RELATION 2 #S(TUPLE 4 5 6) #S(TUPLE 1 2 3))")

;;; test RELATION-ADJOIN function
(let ((relation (empty-relation)))
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

;;; test relation index
(let ((cl-test-more:*default-test-function* #'equalp)
      (relation (empty-relation)))
  ;; adjoin tuples to relation
  (relation-adjoin (tuple 1 1 1) relation)
  (relation-adjoin (tuple 1 1 2) relation)
  (relation-adjoin (tuple 1 2 3) relation)
  (relation-adjoin (tuple 1 2 3) relation)
  ;; test index of the first attribute
  (let ((index (waql::%relation-index relation 0)))
    (is (waql::lookup-relation-index index 1)
        (list (tuple 1 2 3)
              (tuple 1 1 2)
              (tuple 1 1 1)))
    (is (waql::lookup-relation-index index 2) nil))
  ;; test index of the second attribute
  (let ((index (waql::%relation-index relation 1)))
    (is (waql::lookup-relation-index index 1)
        (list (tuple 1 1 2)
              (tuple 1 1 1)))
    (is (waql::lookup-relation-index index 2)
        (list (tuple 1 2 3)))
    (is (waql::lookup-relation-index index 3) nil))
  ;; test index of the third attribute
  (let ((index (waql::%relation-index relation 2)))
    (is (waql::lookup-relation-index index 3)
        (list (tuple 1 2 3))))
  ;; test number too large
  (is-error (waql::%relation-index relation -1) type-error)
  (is-error (waql::%relation-index relation 4) type-error))

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

;;; test I-VAL function
(is (waql::i-val '(1 nil nil)) '(0 1))
(is (waql::i-val '(nil 1 nil)) '(1 1))
(is-error (waql::i-val '(nil nil nil)) simple-error)
(is-error (waql::i-val '(1 1 nil)) simple-error)

;;; test I-VAL-CNT function
(let ((relation (empty-relation)))
  ;; adjoin tuples to relation
  (relation-adjoin (tuple 1 1 1) relation)
  (relation-adjoin (tuple 1 1 2) relation)
  (relation-adjoin (tuple 1 2 3) relation)
  ;; test I-VAL-CNT function
  (is (waql::i-val-cnt '(0 1) relation) '(0 1 3))
  (is (waql::i-val-cnt '(0 2) relation) '(0 2 0))
  (is (waql::i-val-cnt '(1 1) relation) '(1 1 2))
  (is-error (waql::i-val-cnt '(-1 1) relation) type-error)
  (is-error (waql::i-val-cnt '(3 1) relation) type-error))

;;; test MINIMIZE function
(is (waql::minimize '((a 1) (b 2) (c 0)) :key #'cadr) '(c 0))
(is (waql::minimize '()) '())

;;; test RELATION-INDEX-LOOKUP function
(let ((cl-test-more:*default-test-function* #'equalp)
      (relation (empty-relation)))
  ;; adjoin tuples to relation
  (relation-adjoin (tuple 1 1 1) relation)
  (relation-adjoin (tuple 1 1 2) relation)
  (relation-adjoin (tuple 1 2 3) relation)
  (relation-adjoin (tuple 1 2 3) relation)
  ;; test no keys
  (is (waql::relation-index-lookup relation '())
      (list (tuple 1 2 3)
            (tuple 1 1 2)
            (tuple 1 1 1)))
  ;; test key (1 nil nil)
  ;;   order of the result is not assured (compare to above one)
  (is (waql::relation-index-lookup relation '((1 nil nil)))
      (list (tuple 1 2 3)
            (tuple 1 1 2)
            (tuple 1 1 1)))
  ;; test key (nil 1 nil)
  (is (waql::relation-index-lookup relation '((nil 1 nil)))
      (list (tuple 1 1 2)
            (tuple 1 1 1)))
  ;; test key (nil nil 1)
  (is (waql::relation-index-lookup relation '((nil nil 1)))
      (list (tuple 1 1 1)))
  ;; test selective keys returning result using key with least one
  (is (waql::relation-index-lookup relation '((nil 2 nil)
                                              (nil nil 2)))
      (list (tuple 1 2 3))))

;; looking-up empty relation returns nil
(let ((relation (empty-relation)))
  (is (waql::relation-index-lookup relation '((nil 1 nil)))
      nil))

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

(defrelation +r1+ (:user :event)
  (relation-adjoin-all (list (tuple (user 1) (event 1))
                             (tuple (user 1) (event 2))
                             (tuple (user 2) (event 3)))
                       (empty-relation)))

;;; test projection
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u) (<- (u ev) +r1+)) :sexp-p t)))
  (ok (relation-member (tuple (user 1)) result))
  (is (relation-count result) 2))

;;; test selection
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u ev) (<- (u ev) +r1+)
                                       (= (waql::user-id u) 1))
                         :sexp-p t)))
  (ok (relation-member (tuple (user 1) (event 1)) result))
  (ok (null (relation-member (tuple (user 2) (event 3)) result)))
  (is (relation-count result) 2))

;;; test Cartesian product
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u1 ev1 u2 ev2) (<- (u1 ev1) +r1+)
                                                (<- (u2 ev2) +r1+))
                         :sexp-p t)))
  (ok (relation-member (tuple (user 1) (event 1) (user 2) (event 3))
                       result))
  (is (relation-count result) 9))

;;; test natural join
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql
                (query (u1 ev1 ev2) (<- (u1 ev1) +r1+)
                                    (<- (u2 ev2) +r1+)
                                    (= u1 u2)
                                    (< ev1 ev2))
                :sexp-p t)))
  (ok (relation-member (tuple (user 1) (event 1) (event 2)) result))
  (is (relation-count result) 1))

;;; test count aggregation
(let ((result (eval-waql
                (query (u (count (query (ev1) (<- (u ev1) +r1+))))
                       (<- (u ev) +r1+))
                :sexp-p t)))
  (ok (relation-member (tuple (user 1) 2) result))
  (ok (relation-member (tuple (user 2) 1) result))
  (is (relation-count result) 2))

;;; test underscore notation
(let ((result (eval-waql
                (query (u) (<- (u _) +r1+)
                           (<- (u _) +r1+))
                :sexp-p t)))
  (ok (relation-member (tuple (user 1)) result))
  (ok (relation-member (tuple (user 2)) result))
  (is (relation-count result) 2))

;;; test let binding
(let ((result (eval-waql
                (let (x (query (u e) (<- (u e) +r1+)))
                  (let (f ((u :user) (i :int))
                          (= (waql::user-id u) i))
                    (query (u) (<- (u e) x)
                               (f u 1))))
                :sexp-p t)))
  (ok (relation-member (tuple (user 1)) result))
  (is (relation-count result) 1))


;;;
;;; test Solving pattern match
;;;
;;; original: (query (a b c d) (<- (a b c) r1)
;;;                            (<- (a d) r2))
;;;
;;; solved  : (query (a b c d) (<- (a b c) r1)
;;;                            (<- (%a1 d) r2)
;;;                            (= a %a1))
;;;

(diag "test Solving pattern match")

;;; test SOLVE-PATTERN-MATCH function
(is (waql::solve-pattern-match '(query (a b c d) (<- (a b c) r1)
                                                 (<- (a d) r2))
                               (waql::patenv '(r1 r2)))
    '(query (a b c d) (<- (a b c) r1)
                      (<- (%a1 d) r2)
                      (= a %a1)))

;;; test pattern matching in join
(is (waql::solve-pattern-match
      '(query (a b c1) (<- (a b) r1)
                       (<- (a1 c1) (query (a c) (<- (a c) r2))))
      (waql::patenv '(r1 r2)))
    '(query (a b c1) (<- (a b) r1)
                     (<- (a1 c1) (query (a c) (<- (%a1 c) r2)
                                              (= a %a1)))))

;;; test pattern matching in selection
(is (waql::solve-pattern-match
      '(query (a b) (<- (a b) r1)
                    (= (count (query (a c) (<- (a c) r2)))
                       1))
      (waql::patenv '(r1 r2)))
    '(query (a b) (<- (a b) r1)
                  (= (count (query (a c) (<- (%a1 c) r2)
                                         (= a %a1)))
                     1)))

;;; test pattern matching in projection
(is (waql::solve-pattern-match
      '(query (a (count (query (a c) (<- (a c) r2))))
              (<- (a b) r1))
      (waql::patenv '(r1 r2)))
    '(query (a (count (query (a c) (<- (%a1 c) r2)
                                   (= a %a1))))
            (<- (a b) r1)))


;;; test count aggregation
(is (waql::solve-pattern-match
      '(query (a (count (query (a c) (<- (a c) r2))))
              (<- (a b) r1))
      (waql::patenv '(r1 r2)))
    '(query (a (count (query (a c) (<- (%a1 c) r2)
                                   (= a %a1))))
            (<- (a b) r1)))

;;; test underscore notation
(let ((waql::*underscore-count* 1))
  (is (waql::solve-pattern-match
        '(query (a c) (<- (a _) r1)
                      (<- (a c) r2))
        (waql::patenv '(r1 r2)))
      '(query (a c) (<- (a waql::%_1) r1)
                    (<- (%a1 c) r2)
                    (= a %a1))))


;;;
;;; test Solving pattern match - Symbol
;;;

(diag "test Solving pattern match - Symbol")

;;; test SOLVE-PATTERN-MATCH-SYMBOL function
(is (waql::solve-pattern-match-symbol 'a (waql::patenv '(a))) 'a)

;;; predefined relations exist in initial pattern match environment
(is (waql::solve-pattern-match-symbol '+r1+ (waql::initial-patenv)) '+r1+)

;;; error if lookup any symbols in empty pattern match environment
(is-error (waql::solve-pattern-match-symbol '+r1+ (waql::empty-patenv))
          simple-error)

;;; error if symbol does not exist in pattern match environment
(is-error (waql::solve-pattern-match-symbol 'a (waql::empty-patenv))
          simple-error)

;;; symbols starting with "%" are reserved
(is-error (waql::solve-pattern-match-symbol '%a (waql::empty-patenv))
          simple-error)


;;;
;;; test Solving pattern match - Let
;;;

(diag "test Solving pattern match - Let")

;;; test SOLVE-PATTERN-MATCH-LET function

(let ((waql::*underscore-count* 1))
  (is (waql::solve-pattern-match-let '(let (x (query (a b) (<- (a b) r1)
                                                           (<- (a _) r2)))
                                        (let (i 1)
                                          (query (a) (<- (a i) x))))
                                     (waql::patenv '(r1 r2)))
      '(let (x (query (a b) (<- (a b) r1)
                            (<- (%a1 waql::%_1) r2)
                            (= a %a1)))
         (let (i 1)
           (query (a) (<- (a %i1) x)
                        (= i %i1))))))

(is (waql::solve-pattern-match-let '(let (f ((i :int))
                                          (query (a b) (<- (a b) r1)
                                                       (<- (a i) r2)))
                                     (query (a b) (<- (a b) (f 1))))
                                   (waql::patenv '(r1 r2)))
    '(let (f ((i :int)) (query (a b) (<- (a b) r1)
                                     (<- (%a1 %i1) r2)
                                     (= a %a1)
                                     (= i %i1)))
       (query (a b) (<- (a b) (f 1)))))

;;; error if trying to use expression other than symbol on pattern matching
(is-error (waql::solve-pattern-match-let '(let (f ((i :int)) i)
                                            (query (a b) (<- (a (f i)) r)))
                                         (waql::empty-patenv))
          simple-error)


;;;
;;; test Solving pattern match - Query
;;;

(diag "test Solving pattern match - Query")

;;; test SOLVE-PATTERN-MATCH-QUERY function
(is (waql::solve-pattern-match-query '(query (a b c d) (<- (a b c) r1)
                                                       (<- (a d) r2))
                                     (waql::patenv '(r1 r2)))
    '(query (a b c d) (<- (a b c) r1)
                      (<- (%a1 d) r2)
                      (= a %a1)))

;;; test SOLVE-PATTERN-MATCH-QUALS function
(is (waql::solve-pattern-match-quals
      '((<- (a b c) r1) (<- (a d) r2)) '(a b c d)
      (waql::patenv '(r1 r2)))
    '(((<- (a b c) r1) (<- (%a1 d) r2) (= a %a1)) (a b c d)))

;;; test SOLVE-PATTERN-MATCH-QUAL function
(is (waql::solve-pattern-match-qual
      '(<- (a b c) r1) '((<- (a d) r2)) '(a b c d)
      (waql::patenv '(r1 r2)))
  '((<- (a b c) r1) ((<- (%a1 d) r2) (= a %a1)) (a b c d)))


;;;
;;; test Solving pattern match - Query - Quantification
;;;

(diag "test Solving pattern match - Query - Quantification")

;;; test SOLVE-PATTERN-MATCH-QUANTIFICATION function
(is (waql::solve-pattern-match-quantification
      '(<- (a b c) r1) '((<- (a d) r2)) '(a b c d)
      (waql::patenv '(r1 r2)))
    '((<- (a b c) r1) ((<- (%a1 d) r2) (= a %a1)) (a b c d)))

(is (waql::solve-pattern-match-quantification '(<- (a d) r2) nil '(a b c d)
                                              (waql::patenv '(r1 r2 a b c)))
    '((<- (%a1 d) r2) ((= a %a1)) (a b c d)))

(let ((waql::*underscore-count* 1))
  (is (waql::solve-pattern-match-quantification '(<- (_ _) r1) nil '(a b)
                                                (waql::patenv '(r1 a b)))
      '((<- (waql::%_1 waql::%_2) r1) nil (a b))))

(is (waql::solve-pattern-match-quantification '(<- (+r1+) +r1+) nil '(+r1+)
                                              (waql::initial-patenv))
    '((<- (%+r1+1) +r1+) ((= +r1+ %+r1+1)) (+r1+)))

;;; error if trying to use variable starting with "%"
(is-error (waql::solve-pattern-match-quantification '(<- (%a) a) nil nil
                                                    (waql::patenv '(a)))
          simple-error)

;; error if trying to use duplicated variables
(is-error (waql::solve-pattern-match-quantification '(<- (a a) r) nil nil
                                                    (waql::patenv '(r)))
          simple-error)


;;;
;;; test Solving pattern match - Query - Predicate
;;;

(diag "test Solving pattern match - Query - Predicate")


;;;
;;; test Solving pattern match - Function application
;;;

(diag "test Solving pattern match - Function application")

;;; test SOLVE-PATTERN-MATCH-FUNCTION function
(is (waql::solve-pattern-match-function '(= u u1) (waql::patenv '(u u1)))
    '(= u u1))

(is (waql::solve-pattern-match-function '(count r) (waql::patenv '(r)))
    '(count r))


;;;
;;; test Solving pattern match - Pattern matcher
;;;

(diag "test Solving pattern match - Pattern matcher")

;;; test MAKE-PATTERN-MATCHER constructor and PATTERN-MATCHER-RESULT selector
(let ((patenv (waql::empty-patenv)))
  (let ((matcher (waql::make-pattern-matcher patenv)))
    (destructuring-bind (vars patenv1 preds)
        (waql::pattern-matcher-result matcher)
      (is vars nil)
      (is (waql::lookup-patenv 'a patenv1) nil)
      (is preds nil))))

;;; test PATTERN-MATCHER-MATCH function
(let ((patenv (waql::add-patenv 'b
                (waql::add-patenv 'a (waql::empty-patenv)))))
  (let* ((waql::*underscore-count* 1)
         (matcher (waql::pattern-matcher-match '_
                    (waql::pattern-matcher-match '_
                      (waql::pattern-matcher-match 'c
                        (waql::pattern-matcher-match 'b
                          (waql::pattern-matcher-match 'a
                            (waql::make-pattern-matcher patenv))))))))
    (destructuring-bind (vars patenv1 preds)
        (waql::pattern-matcher-result matcher)
      (is vars '(%a1 %b1 c waql::%_1 waql::%_2))
      (is (waql::lookup-patenv 'a patenv1) '(a . 2))
      (is (waql::lookup-patenv 'b patenv1) '(b . 2))
      (is (waql::lookup-patenv 'c patenv1) '(c . 1))
      (is preds '((= a %a1)
                  (= b %b1))))))

(let* ((patenv (waql::initial-patenv))
       (matcher (waql::pattern-matcher-match '+r1+
                  (waql::make-pattern-matcher patenv))))
  (destructuring-bind (vars patenv1 preds)
      (waql::pattern-matcher-result matcher)
    (is vars '(%+r1+1))
    (is (waql::lookup-patenv '+r1+ patenv1) '(+r1+ . 2))
    (is preds '((= +r1+ %+r1+1)))))

;;; error if matched form is not valid symbol
(is-error (waql::pattern-matcher-match t
            (waql::make-pattern-matcher (waql::empty-patenv)))
          simple-error)
(is-error (waql::pattern-matcher-match nil
            (waql::make-pattern-matcher (waql::empty-patenv)))
          simple-error)
(is-error (waql::pattern-matcher-match '(f i)
            (waql::make-pattern-matcher (waql::empty-patenv)))
          simple-error)

;;; test UNIQUE-SYMBOL function
(is (waql::unique-symbol 'a 1) '%a1)
(is-error (waql::unique-symbol 1 1) simple-error)
(is-error (waql::unique-symbol 'a 'a) simple-error)
(is-error (waql::unique-symbol 't 1) simple-error)
(is-error (waql::unique-symbol nil 1) simple-error)
(is-error (waql::unique-symbol '() 1) simple-error)

;;; test ORIGINAL-SYMBOL function
(let ((a1 (waql::unique-symbol 'a 1)))
  (is (waql::original-symbol a1) 'a))

(let ((_1 (waql::unique-symbol '_ 1)))
  (is (waql::original-symbol _1) '_))

(is (waql::original-symbol 'b) nil)

;;; test PATTERN-MATCHER-MATCH-ALL function
(let ((patenv (waql::add-patenv 'b
                (waql::add-patenv 'a (waql::empty-patenv)))))
  (let ((matcher (waql::pattern-matcher-match-all '(a b c)
                   (waql::make-pattern-matcher patenv))))
    (destructuring-bind (vars patenv1 preds)
        (waql::pattern-matcher-result matcher)
      (is vars '(%a1 %b1 c))
      (is (waql::lookup-patenv 'a patenv1) '(a . 2))
      (is (waql::lookup-patenv 'b patenv1) '(b . 2))
      (is (waql::lookup-patenv 'c patenv1) '(c . 1))
      (is preds '((= a %a1)
                  (= b %b1))))))


;;;
;;; test Solving pattern match - Pattern matching environment
;;;

(diag "test Solving pattern match - Pattern matching environment")

;;; test EMPTY-PATENV constructor and LOOKUP-PATENV function
(ok (null (waql::lookup-patenv 'a (waql::empty-patenv))))

;;; test INITIAL-PATENV function
(ok (waql::lookup-patenv '+r1+ (waql::initial-patenv)))
(ok (null (waql::lookup-patenv '+r1+ (waql::empty-patenv))))

;;; test ADD-PATENV function
(let ((patenv (waql::add-patenv 'b
                (waql::add-patenv 'a (waql::empty-patenv)))))
  (is (waql::lookup-patenv 'a patenv) '(a . 1))
  (is (waql::lookup-patenv 'b patenv) '(b . 1)))

;;; error if trying to add duplicated symbol
(let ((patenv (waql::add-patenv 'a (waql::empty-patenv))))
  (is-error (waql::add-patenv 'a patenv) simple-error))

;;; test INC-PATENV function
(let ((patenv (waql::inc-patenv 'a
                (waql::add-patenv 'b
                  (waql::add-patenv 'a (waql::empty-patenv))))))
  (is (waql::lookup-patenv 'a patenv) '(a . 2)))

;;; error if trying to increment non-exist symbol
(let ((patenv (waql::add-patenv 'a (waql::empty-patenv))))
  (is-error (waql::inc-patenv 'b patenv) simple-error))

;;; test PRINT-PATENV function
(let ((patenv (waql::inc-patenv 'a
                (waql::add-patenv 'b
                  (waql::add-patenv 'a (waql::empty-patenv))))))
  (is-print (print-object patenv *standard-output*)
            "#S(WAQL::PATENV (B . 1) (A . 2))"))


;;;
;;; test Function specialization
;;;

(diag "test Function specialization")


;;;
;;; test Function specialization - Literal
;;;

(diag "test Function specialization - Literal")

;;; test SPECIALIZE-FUNCTION-LITERAL function
(is (waql::specialize-function-literal 1) '(1 :int))
(is (waql::specialize-function-literal "foo") '("foo" :string))
(is (waql::specialize-function-literal '(time "2013/1/1" "10:10:35"))
    '((time "2013/1/1" "10:10:35") :time))
(is-error (waql::specialize-function-literal 'a) simple-error)


;;;
;;; test Function specialization - Symbol
;;;

(diag "test Function specialization - Symbol")

;;; test SPECIALIZE-FUNCTION-SYMBOL function
(let ((typenv (waql::add-typenv 'a :user (waql::empty-typenv))))
  (is (waql::specialize-function-symbol 'a typenv) '(a :user)))

;;; error if symbol is not bound
(let ((typenv (waql::empty-typenv)))
  (is-error (waql::specialize-function-symbol 'a typenv) simple-error))

;;; error if symbol is bound to function
(let ((typenv (waql::add-typenv 'f '(:function (:int) :user)
                (waql::empty-typenv))))
  (is-error (waql::specialize-function-symbol 'f typenv) simple-error))

;;; test to lookup predefined relations
(let ((typenv (waql::empty-typenv))
      (waql::*predefined-relations*
        (waql::add-predefined-relations 'r '(:user :event)
          (waql::make-predefined-relations))))
  (is (waql::specialize-function-symbol 'r typenv)
      '(r (:relation :user :event))))


;;;
;;; test Function specialization - Let
;;;

(diag "test Function specialization - Let")

(let ((waql::*predefined-relations*
        (waql::add-predefined-relations 'r1 '(:user)
          (waql::make-predefined-relations))))
  (is (waql::specialize-function-let '(let (x (waql::user 1))
                                        (query (a) (<- (a) r1)
                                                   (= a x)))
                                     (waql::empty-typenv))
      '((let (x (user 1))
          (query (a) (<- (a) r1)
                     (waql::user= a x)))
        (:relation :user))))


(let ((waql::*predefined-relations*
        (waql::add-predefined-relations 'r1 '(:user)
          (waql::make-predefined-relations))))
  (is (waql::specialize-function-let '(let (f ((i :int)) (user i))
                                        (query (a) (<- (a) r1)
                                                   (= a (f 1))))
                                     (waql::empty-typenv))
      '((let (f ((i :int)) (user i))
          (query (a) (<- (a) r1)
                     (waql::user= a (f 1))))
        (:relation :user))))


;;;
;;; test Function specialization - Query
;;;

(diag "test Function specialization - Query")

;;; test SPECIALIZE-FUNCTION-QUERY function
(let ((typenv (waql::empty-typenv))
      (waql::*predefined-relations*
        (waql::add-predefined-relations 'r2 '(:user :event)
          (waql::add-predefined-relations 'r1 '(:user :event)
            (waql::make-predefined-relations)))))
  (is (waql::specialize-function-query '(query (a b c) (<- (a b) r1)
                                                       (<- (%a1 c) r2)
                                                       (= a %a1))
                                       typenv)
      '((query (a b c) (<- (a b) r1)
                       (<- (%a1 c) r2)
                       (waql::user= a %a1))
        (:relation :user :event :event))))


;;;
;;; test Function specialization - Query - Quantification
;;;

(diag "test Function specialization - Query - Quantification")

;;; test SPECIALIZE-FUNCTION-QUANTIFICATION function
(let ((typenv (waql::add-typenv 'x :int
                (waql::empty-typenv))))
  (is-error (waql::specialize-function-quantification
              '(waql:<- (a b) x) nil '(a b) typenv)
            simple-error))


;;;
;;; test Function specialization - Lisp form
;;;

(diag "test Function specialization - Lisp form")

;;; test SPECIALIZE-FUNCTION-LISP-FORM function
(is (waql::specialize-function-lisp-form '(lisp 'some-lisp-form))
    '((lisp 'some-lisp-form) :bool))


;;;
;;; test Function specialization - Function application
;;;

(diag "test Function specialization - Function application")

;;; test SPECIALIZE-FUNCTION-FUNCTION function
(let ((patenv (waql::add-typenv '%a1 :user
                (waql::add-typenv 'a :user
                  (waql::empty-typenv)))))
  (is (waql::specialize-function-function '(= a %a1) patenv)
      '((waql::user= a %a1) :bool)))


;;;
;;; test Function specialization - Function table
;;;

(diag "test Function specialization - Function table")

;;; test LOOKUP-GENERIC-FUNCTION function

(is (waql::lookup-generic-function '= '(:user :user))
    '(:bool waql::user=))

(is (waql::lookup-generic-function '= '(:string :string))
    '(:bool string=))

(is (waql::lookup-generic-function '= '(:time :time))
    '(:bool local-time:timestamp=))

(is (waql::lookup-generic-function '< '(:time :time))
    '(:bool local-time:timestamp<))

(is (waql::lookup-generic-function '> '(:time :time))
    '(:bool local-time:timestamp>))

(is (waql::lookup-generic-function 'foo nil) nil)


;;;
;;; test Function specialization - Type environment
;;;

(diag "test Function specialization - Type environment")

;;; test basic interfaces of Type environment
(let ((typenv (waql::add-typenv 'b :event
                (waql::add-typenv 'a :user
                  (waql::empty-typenv)))))
  (is (waql::lookup-typenv 'a typenv) :user)
  (is (waql::lookup-typenv 'b typenv) :event)
  (ok (null (waql::lookup-typenv 'c typenv)))
  (let ((typenv1 (waql::add-typenv 'a :int typenv)))
    (is (waql::lookup-typenv 'a typenv1) :int)))

;;; test REMOVE-TYPENV function
(let ((typenv (waql::remove-typenv 'a
                (waql::add-typenv 'a :user
                  (waql::empty-typenv)))))
  (ok (null (waql::lookup-typenv 'a typenv))))


;;;
;;;
;;; test Compiler
;;;

(diag "test Compiler")

(is (waql::compile-expression-top
      '(query (a2 c) (<- (a b) +r1+)
                     (<- (a2 c) (query (a1 c) (<- (a1 c) +r1+)
                                              (waql::user= a a1)))))
    '(iterate:iter waql::outermost
       (for-tuple (a b) in-relation +r1+)
         (iterate:iter (for-tuple (a2 c) in-relation
                         (iterate:iter waql::outermost
                           (for-tuple (a1 c) in-relation +r1+)
                           (when (waql::user= a a1)
                             (iterate:in waql::outermost
                               (collect-relation (tuple a1 c))))))
                       (iterate:in waql::outermost
                         (collect-relation (tuple a2 c))))))

;;; query in binder relation of quantification
;;;   deriving lookup-keys
(let ((%u (waql::unique-symbol 'u 1)))
  (is (waql::compile-expression-top
       `(let (u (user 1))
          (query (u e) (<- (,%u e)
            (query (u2 e) (<- (u2 e)
              (query (u3 e) (<- (u3 e) +r1+))))))))
      '(iterate:iter waql::outermost
         (for-tuple (%u1 e)
           in-relation (iterate:iter waql::outermost
                         (for-tuple (u2 e)
                           in-relation
                             (iterate:iter waql::outermost
                               (for-tuple (u3 e) in-relation +r1+
                                 using (list (list (user 1) nil)))
                               (iterate:in waql::outermost
                                 (collect-relation (tuple u3 e))))
                           using (list (list (user 1) nil)))
                         (iterate:in waql::outermost
                           (collect-relation (tuple u2 e))))
           using (list (list (user 1) nil)))
        (iterate:in waql::outermost
          (collect-relation (tuple (user 1) e))))))

;;; symbol in binder relation of quantification
;;;   deriving lookup-keys
(let ((waql::*scoping-count* 1)
      (%u (waql::unique-symbol 'u 1)))
  (is (waql::compile-expression-top
       `(let (+r2+ (query (u2 e) (<- (u2 e) +r1+)))
          (let (u (user 1))
            (query (u e) (<- (,%u e) +r2+)))))
      `(iterate:iter waql::outermost
         (for-tuple (%u1 e)
           in-relation (iterate:iter waql::outermost
                         (for-tuple (%+r2+1.u2 %+r2+1.e)
                           in-relation +r1+
                           using (list (list (user 1) nil)))
                         (iterate:in waql::outermost
                           (collect-relation
                             (tuple %+r2+1.u2 %+r2+1.e))))
           using (list (list (user 1) nil)))
         (iterate:in waql::outermost
           (collect-relation (tuple (user 1) e))))))

;;; let-binded function application in binder relation of quantification
;;;   not deriving lookup-keys
(let ((waql::*scoping-count* 1)
      (%u (waql::unique-symbol 'u 1)))
  (is (waql::compile-expression-top
        `(let (f ((i :int)) (query (u2 e) (<- (u2 e) +r1+)))
           (let (u (user 1))
             (query (u e) (<- (,%u e) (f 1))))))
      '(iterate:iter waql::outermost
         (for-tuple (%u1 e)
           in-relation (iterate:iter waql::outermost
                         (for-tuple (%f1.u2 %f1.e) in-relation +r1+)
                         (iterate:in waql::outermost
                           (collect-relation (tuple %f1.u2 %f1.e))))
           using (list (list (user 1) nil)))
         (iterate:in waql::outermost
           (collect-relation (tuple (user 1) e))))))

;;; let variable binding in binder relation of quantification
;;;   not deriving lookup-keys
(let ((waql::*scoping-count* 1)
      (%u (waql::unique-symbol 'u 1)))
  (is (waql::compile-expression-top
        `(let (u (user 1))
           (query (u e) (<- (,%u e) (let (x 1)
                                      (query (u1 e) (<- (u1 e) +r1+)))))))
      '(iterate:iter waql::outermost
         (for-tuple (%u1 e)
           in-relation (iterate:iter waql::outermost
                         (for-tuple (u1 e) in-relation +r1+)
                         (iterate:in waql::outermost
                           (collect-relation (tuple u1 e))))
           using (list (list (user 1) nil)))
         (iterate:in waql::outermost
           (collect-relation (tuple (user 1) e))))))

;;; let variable binding in expressions of sub query
;;;   not deriving lookup-keys
(let ((waql::*scoping-count* 1)
      (%u (waql::unique-symbol 'u 1)))
  (is (waql::compile-expression-top
        `(let (u (user 1))
           (query (u e) (<- (,%u e) (query ((let (x 1)
                                          (user 1))
                                        e)
                                       (<- (u1 e) +r1+))))))
      '(iterate:iter waql::outermost
        (for-tuple (%u1 e)
          in-relation (iterate:iter waql::outermost
                        (for-tuple (u1 e) in-relation +r1+)
                        (iterate:in waql::outermost
                          (collect-relation (tuple (user 1) e))))
          using (list (list (user 1) nil)))
        (iterate:in waql::outermost
          (collect-relation (tuple (user 1) e))))))

;;; case that sub query has several quantifications
;;;   deriving lookup-keys to each quantification
(let ((waql::*scoping-count* 1)
      (%u (waql::unique-symbol 'u 1))
      (%u1 (waql::unique-symbol 'u1 2)))
  (is (waql::compile-expression-top
        `(let (u (user 1))
           (query (u e) (<- (,%u e) (query (u1 e1) (<- (u1 e1) +r1+)
                                                   (<- (,%u1 e2) +r1+))))))
      '(iterate:iter waql::outermost
         (for-tuple (%u1 e)
           in-relation (iterate:iter waql::outermost
                         (for-tuple (u1 e1)
                           in-relation +r1+
                           using (list (list (user 1) nil)))
                         (iterate:iter
                           (for-tuple (%u12 e2)
                             in-relation +r1+
                             using (list (list u1 nil)))
                           (iterate:in waql::outermost
                             (collect-relation (tuple u1 e1)))))
           using (list (list (user 1) nil)))
        (iterate:in waql::outermost
          (collect-relation (tuple (user 1) e))))))


;;;
;;; test Compiler - Literal
;;;

(diag "test Compiler - Literal")

;;; test COMPILE-LITERAL-TIME function
(is (waql::compile-literal-time '(time "2013/1/1" "10:10:35"))
    '(local-time:parse-timestring "2013/1/1T10:10:35"))


;;;
;;; test Compiler - Symbol
;;;

(diag "test Compiler - Symbol")

;;; test COMPILE-SYMBOL function

(let ((compenv (waql::add-qvar-compenv 'a
                 (waql::empty-compenv))))
  (is (waql::compile-symbol 'a compenv '%a1 nil) '%a1.a))

(let ((compenv (waql::add-argvar-compenv 'a 1
                 (waql::empty-compenv))))
  (is (waql::compile-symbol 'a compenv nil nil) 1))

(let ((waql::*scoping-count* 1)
      (compenv (waql::add-letvar-compenv 'a '(query (a b) (<- (a b) +r1+))
                 (waql::empty-compenv))))
  (is (waql::compile-symbol 'a compenv '%a1 nil)
        '(iterate:iter waql::outermost
           (for-tuple (%a1.a %a1.b) in-relation +r1+)
           (iterate:in waql::outermost
             (collect-relation (tuple %a1.a %a1.b))))))

(let ((compenv (waql::add-letfun-compenv 'f '(e) '(query (a) (<- (a e) +r1+))
                 (waql::empty-compenv))))
  (is-error (waql::compile-symbol 'f compenv nil nil) simple-error))

(let ((compenv (waql::empty-compenv)))
  (is-error (waql::compile-symbol 'a compenv nil nil) simple-error))


;;;
;;; test Compiler - Let
;;;

(diag "test Compiler - Let")

;;; test COMPILE-LET function

(let ((waql::*scoping-count* 1))
  (is (waql::compile-let '(let (r +r1+)
                             (let (x (query (a b) (<- (a b) r)))
                               (query (c d) (<- (c d) x))))
                          (waql::empty-compenv) nil)
      '(iterate:iter waql::outermost
        (for-tuple (c d) in-relation
          (iterate:iter waql::outermost
            (for-tuple (%X1.a %X1.b) in-relation +r1+)
            (iterate:in waql::outermost
               (collect-relation (tuple %X1.a %X1.b)))))
        (iterate:in waql::outermost
          (collect-relation (tuple c d))))))

(let ((waql::*scoping-count* 1))
  (is (waql::compile-let '(let (r +r1+)
                             (let (f ((i :int)) (query (a b) (<- (a b) r)
                                                             (= a i)))
                               (query (c d) (<- (c d) (f 1)))))
                          (waql::empty-compenv) nil)
      '(iterate:iter waql::outermost
         (for-tuple (c d) in-relation
           (iterate:iter waql::outermost
             (for-tuple (%F1.a %F1.b) in-relation +r1+)
             (when (= %F1.a 1)
               (iterate:in waql::outermost
                 (collect-relation (tuple %F1.a %F1.b))))))
         (iterate:in waql::outermost
           (collect-relation (tuple c d))))))


;;;
;;; test Compiler - Query
;;;

(diag "test Compiler - Query")

;;; test COMPILE-QUERY function

(let ((waql::*scoping-count* 1)
      (compenv (waql::add-letvar-compenv 'x '(query (a b) (<- (a b) +r1+))
                 (waql::empty-compenv))))
  (is (waql::compile-query '(query (a b) (<- (a b) x)
                                         (<- (c d) x))
                           compenv nil nil)
       '(iterate:iter waql::outermost
          (for-tuple (a b) in-relation
              (iterate:iter waql::outermost
                (for-tuple (%x1.a %x1.b) in-relation +r1+)
                (iterate:in waql::outermost
                  (collect-relation (tuple %x1.a %x1.b)))))
          (iterate:iter
            (for-tuple (c d) in-relation
                (iterate:iter waql::outermost
                  (for-tuple (%x2.a %x2.b) in-relation +r1+)
                  (iterate:in waql::outermost
                    (collect-relation (tuple %x2.a %x2.b)))))
            (iterate:in waql::outermost
              (collect-relation (tuple a b)))))))

(let ((waql::*scoping-count* 1)
      (compenv (waql::add-letvar-compenv 'y '(query (a b) (<- (a b) x))
                 (waql::add-letvar-compenv 'x '(query (a b) (<- (a b) +r1+))
                   (waql::empty-compenv)))))
  (is (waql::compile-query '(query (a c) (<- (a b) x)
                                         (<- (c d) y))
                           compenv nil nil)
      '(iterate:iter waql::outermost
         (for-tuple (a b) in-relation
             (iterate:iter waql::outermost
               (for-tuple (%x1.a %x1.b) in-relation +r1+)
               (iterate:in waql::outermost
                 (collect-relation
                   (tuple %x1.a %x1.b)))))
           (iterate:iter
             (for-tuple (c d) in-relation
                 (iterate:iter waql::outermost
                   (for-tuple (%Y2.a %Y2.b) in-relation
                     (iterate:iter waql::outermost
                       (for-tuple (%X3.a %X3.b) in-relation +r1+)
                       (iterate:in waql::outermost
                         (collect-relation (tuple %X3.a %X3.b)))))
                   (iterate:in waql::outermost
                     (collect-relation (tuple %Y2.a %Y2.b)))))
         (iterate:in waql::outermost
           (collect-relation (tuple a c)))))))

;;; test COMPILE-QUERY-QUALS function

(is (waql::compile-query-quals '((<- (a b) +r1+)) '(a b)
                               (waql::empty-compenv) nil nil :outermost t)
    '(iterate:iter waql::outermost
       (for-tuple (a b) in-relation +r1+)
         (iterate:in waql::outermost
           (collect-relation (tuple a b)))))

(is (waql::compile-query-quals '((<- (a b) +r1+)
                                 (<- (c d) +r1+)
                                 (waql::user= a c))
                               '(a b c d)
                               (waql::empty-compenv) nil nil :outermost t)
    `(iterate:iter waql::outermost
       (for-tuple (a b) in-relation +r1+)
         (iterate:iter (for-tuple (c d) in-relation +r1+)
           (when (waql::user= a c)
             (iterate:in waql::outermost
               (collect-relation (tuple a b c d)))))))

;;; test COMPILE-QUERY-QUAL function
(is (waql::compile-query-qual '(<- (a b) +r1+) nil '(a b)
                              (waql::empty-compenv) nil nil t)
    '(iterate:iter waql::outermost
       (for-tuple (a b) in-relation +r1+)
         (iterate:in waql::outermost
           (collect-relation (tuple a b)))))

;;; test COMPILE-QUERY-EXPRS function
(let ((compenv (waql::add-qvar-compenv 'c
                 (waql::add-qvar-compenv 'b
                   (waql::add-qvar-compenv 'a
                     (waql::empty-compenv))))))
  (is (waql::compile-query-exprs '(a b c) compenv nil)
      '(iterate:in waql::outermost
         (collect-relation (tuple a b c)))))

(is-error (waql::compile-query-exprs 'a (waql::empty-compenv) nil) type-error)


;;;
;;; test Compiler - Query - Quantification
;;;

(diag "test Compiler - Query - Quantification")

;;; test ORIGINAL-VARS function

(let ((%a (waql::unique-symbol 'a 1))
      (%b (waql::unique-symbol 'b 2))
      (%_ (waql::unique-symbol '_ 3)))
  (is (waql::original-vars (list 'x %a %b %_)) '(nil a b nil)))

(is-error (waql::original-vars 1) type-error)

(is-error (waql::original-vars '(1)) type-error)


;;; test KEYS-FROM-CURRENT-QUANTIFICATION function

(let ((%a (waql::unique-symbol 'a 1))
      (%b (waql::unique-symbol 'b 2))
      (%_ (waql::unique-symbol '_ 3)))
  (let ((compenv (waql::empty-compenv)))
    (let ((keys (waql::keys-from-current-quantification (list 'x %a %b %_)
                                                        compenv 'foo)))
      (let ((key (first keys)))
        (is (waql::lookup-key-elements key) '(nil a nil nil))
        (is (waql::lookup-key-compenv key) compenv)
        (is (waql::lookup-key-scope key) 'foo))
      (let ((key (second keys)))
        (is (waql::lookup-key-elements key) '(nil nil b nil))
        (is (waql::lookup-key-compenv key) compenv)
        (is (waql::lookup-key-scope key) 'foo))
      (= (length keys) 2))))

(let ((compenv (waql::empty-compenv)))
  (is-error (waql::keys-from-current-quantification nil compenv 'foo)
            simple-error))

(let ((compenv (waql::empty-compenv)))
  (is-error (waql::keys-from-current-quantification 1 compenv 'foo)
            type-error))

(let ((compenv (waql::empty-compenv)))
  (is-error (waql::keys-from-current-quantification '(x 1) compenv 'foo)
            simple-error))

(let ((%a (waql::unique-symbol 'a 1)))
  (is-error (waql::keys-from-current-quantification (list %a) 1 'foo)
            type-error))

(let ((%a (waql::unique-symbol 'a 1))
      (compenv (waql::empty-compenv)))
  (is-error (waql::keys-from-current-quantification (list %a) compenv 1)
            simple-error))


;;; test KEY-FROM-ASCENT-QUANTIFICATION function

(let ((compenv (waql::empty-compenv)))
  (let ((key (waql::make-lookup-key '(a nil) compenv nil)))
    (let ((key1 (waql::key-from-ascent-quantification key '(y x) '(x y))))
      (is (waql::lookup-key-elements key1) '(nil a))
      (is (waql::lookup-key-compenv key1) compenv)
      (is (waql::lookup-key-scope key1) nil))))

(let ((compenv (waql::empty-compenv)))
  (let ((key (waql::make-lookup-key '(a nil) compenv nil)))
    (let ((key1 (waql::key-from-ascent-quantification key '(y x) '(a b))))
      (is key1 nil))))

(let ((compenv (waql::empty-compenv)))
  (let ((key (waql::make-lookup-key '(a nil) compenv nil)))
    (let ((key1 (waql::key-from-ascent-quantification key '((let (y 1) y) x)
                                                      '(x y))))
      (is key1 nil))))

(let ((compenv (waql::empty-compenv)))
  (let ((key (waql::make-lookup-key '(a nil) compenv nil)))
    (is-error (waql::key-from-ascent-quantification key 1 '(x y))
              simple-error)
    (is-error (waql::key-from-ascent-quantification key '(y x) 1)
              simple-error)
    (is-error (waql::key-from-ascent-quantification key '(y x) '(1))
              simple-error)))


;;; test KEYS-FROM-ASCENT-QUANTIFICATION function


(let ((compenv (waql::empty-compenv)))
  (let ((keys (list (waql::make-lookup-key '(a nil) compenv nil))))
    (let ((keys1 (waql::keys-from-ascent-quantification keys '(y x) '(x y))))
      (let ((key1 (first keys1)))
        (is (waql::lookup-key-elements key1) '(nil a))
        (is (waql::lookup-key-compenv key1) compenv)
        (is (waql::lookup-key-scope key1) nil)))))

(let ((compenv (waql::empty-compenv)))
  (let ((keys (list (waql::make-lookup-key '(a nil) compenv nil))))
    (let ((keys1 (waql::keys-from-ascent-quantification
                   keys '((let (y 1) y) x) '(x y))))
      (is keys1 nil))))

(let ((compenv (waql::empty-compenv)))
  (let ((keys (list (waql::make-lookup-key '(a nil) compenv nil))))
    (let ((keys1 (waql::keys-from-ascent-quantification keys '(y x) '(x z))))
      (is keys1 nil))))


;;; test COMPILE-QUANTIFICATION function

(is (waql::compile-quantification '(<- (a b) +r1+) nil '(a b)
                                  (waql::empty-compenv) nil nil t)
    '(iterate:iter waql::outermost
       (for-tuple (a b) in-relation +r1+)
       (iterate:in waql::outermost
         (collect-relation (tuple a b)))))

(is (waql::compile-quantification '(<- (a b) +r1+) nil '(a b)
                                  (waql::empty-compenv) nil nil nil)
    '(iterate:iter
       (for-tuple (a b) in-relation +r1+)
       (iterate:in waql::outermost
         (collect-relation (tuple a b)))))

(is-error (waql::compile-quantification '(<- (a b) +r1+) nil '(a b c)
                                        (waql::empty-compenv) nil nil t)
          simple-error)

(let ((%a (waql::unique-symbol 'a 1)))
  (let ((compenv (waql::add-qvar-compenv 'x
                   (waql::add-qvar-compenv 'a
                     (waql::empty-compenv)))))
    (let ((lookup-keys (list (waql::make-lookup-key '(x nil) compenv nil))))
      (is (waql::compile-quantification `(<- (,%a b) +r1+) nil `(b ,%a)
                                        compenv nil lookup-keys t)
          `(iterate:iter waql::outermost
             (for-tuple (%a1 b) in-relation +r1+ using (list (list nil x)
                                                             (list a nil)))
             (iterate:in waql::outermost
               (collect-relation (tuple b %a1))))))))

(let ((waql::*scoping-count* 1)
      (%x (waql::unique-symbol 'x 1)))
  (let ((compenv (waql::add-letvar-compenv
                   'x '(user (relation-count (query (a) (<- (a b) +r1+))))
                   (waql::empty-compenv))))
    (is (waql::compile-quantification `(<- (,%x y) +r1+) nil `(,%x y)
                                      compenv nil nil t)
        '(iterate:iter waql::outermost
           (for-tuple (%x1 y) in-relation +r1+
             using (list (list (user (relation-count
                                       (iterate:iter waql::outermost
                                         (for-tuple (%x1.a %x1.b)
                                           in-relation +r1+)
                                         (iterate:in waql::outermost
                                           (collect-relation
                                             (tuple %x1.a))))))
                               nil)))
          (iterate:in waql::outermost
            (collect-relation
              (tuple %x1 y)))))))


;;;
;;; test Compiler - Query - Quantification - Symbol-Expr pair
;;;

(diag "test Compiler - Query - Quantification - Symbol-Expr pair")


;;; test SYMBOL-EXPR-PAIR function

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(nil a nil)
                                           compenv nil)))
    (is (waql::symbol-expr-pair '(nil x nil) lookup-key) '(x . a))))

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(nil a nil)
                                           compenv nil)))
    (is (waql::symbol-expr-pair '(nil (let (x 1) x) nil) lookup-key) nil)))

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(nil a nil) compenv nil)))
    (is-error (waql::symbol-expr-pair 1 lookup-key) simple-error)))

(is-error (waql::symbol-expr-pair '(nil x nil) 1) simple-error)

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(nil a nil) compenv nil)))
    (is-error (waql::symbol-expr-pair '(nil x) lookup-key) simple-error)))


;;; test LOOKUP-KEY-WITH-SYMBOL-EXPR-PAIR function

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::lookup-key-with-symbol-expr-pair
                      '(x . a) '(x y) compenv 'foo)))
    (is (waql::lookup-key-elements lookup-key) '(a nil))
    (is (waql::lookup-key-compenv lookup-key) compenv)
    (is (waql::lookup-key-scope lookup-key) 'foo)))

(let ((compenv (waql::empty-compenv)))
  (is (waql::lookup-key-with-symbol-expr-pair nil '(x y) compenv 'foo)
      nil))

(let ((compenv (waql::empty-compenv)))
  (is (waql::lookup-key-with-symbol-expr-pair '(z . a) '(x y) compenv 'foo)
      nil))

(let ((compenv (waql::empty-compenv)))
  (is-error (waql::lookup-key-with-symbol-expr-pair '(x . a) 1 compenv 'foo)
            simple-error))

(let ((compenv (waql::empty-compenv)))
  (is-error (waql::lookup-key-with-symbol-expr-pair 1 '(x y) compenv 'foo)
            simple-error))


;;;
;;; test Compiler - Query - Quantification - Lookup key
;;;

(diag "test Compiler - Query - Quantification - Lookup key")

;;; test LOOKUP-KEY-ELEMENTS-P function

(is (waql::lookup-key-elements-p '(x nil nil)) t)
(is (waql::lookup-key-elements-p 'x) nil)
(is (waql::lookup-key-elements-p '(nil nil nil)) nil)
(is (waql::lookup-key-elements-p '(x x nil)) nil)
(is (waql::lookup-key-elements-p '((let (x 1) x) nil nil)) nil)


;;; test MAKE-LOOKUP-KEY function

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(x nil nil) compenv 'foo)))
    (is (waql::lookup-key-elements lookup-key) '(x nil nil))
    (is (waql::lookup-key-compenv lookup-key) compenv)
    (is (waql::lookup-key-scope lookup-key) 'foo)))

(let ((compenv (waql::empty-compenv)))
  (is-error (waql::make-lookup-key '(nil nil nil) compenv 'foo)
            simple-error))

(is-error (waql::make-lookup-key '(x nil nil) 'x 'foo) simple-type-error)

(let ((compenv (waql::empty-compenv)))
  (is-error (waql::make-lookup-key '(x nil nil) compenv 1) simple-error))


;;; test LOOKUP-KEY-P function

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(x nil nil) compenv 'foo)))
    (waql::lookup-key-p lookup-key)))

(is (waql::lookup-key-p 1) nil)


;;; test LOOKUP-KEY-ELEMENTS function

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(x nil nil) compenv 'foo)))
    (is (waql::lookup-key-elements lookup-key) '(x nil nil))))

(is-error (waql::lookup-key-elements 1) simple-error)


;;; test LOOKUP-KEY-DIMENSION function

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(x nil nil) compenv 'foo)))
    (is (waql::lookup-key-dimension lookup-key) 3)))

(is-error (waql::lookup-key-dimension 1) simple-error)


;;; test LOOKUP-KEY-COMPENV function

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(x nil nil) compenv 'foo)))
    (is (waql::lookup-key-compenv lookup-key) compenv)))

(is-error (waql::lookup-key-compenv 1) simple-error)


;;; test LOOKUP-KEY-SCOPE function

(let ((compenv (waql::empty-compenv)))
  (let ((lookup-key (waql::make-lookup-key '(x nil nil) compenv 'foo)))
    (is (waql::lookup-key-scope lookup-key) 'foo)))

(is-error (waql::lookup-key-scope 1) simple-error)


;;; test COMPILE-LOOKUP-KEY function

(let ((compenv (waql::add-letvar-compenv 'x '(let (y 1) y)
                 (waql::empty-compenv))))
  (let ((lookup-key (waql::make-lookup-key '(x nil nil) compenv 'foo)))
    (is (waql::compile-lookup-key lookup-key)
        '(list 1 nil nil))))

(is-error (waql::compile-lookup-key 1) simple-error)


;;; test COMPILE-LOOKUP-KEYS function

(let ((compenv (waql::add-qvar-compenv 'x
                 (waql::add-qvar-compenv 'y
                   (waql::empty-compenv)))))
  (let ((lookup-keys (list (waql::make-lookup-key '(x nil nil) compenv 'foo)
                           (waql::make-lookup-key '(nil y nil) compenv 'foo))))
    (is (waql::compile-lookup-keys lookup-keys)
        '(list (list foo.x nil nil)
               (list nil foo.y nil)))))

(is (waql::compile-lookup-keys nil) '(list))


;;;
;;; test Compiler - Query - Predicate
;;;

(diag "test Compiler - Query - Predicate")

;;; test COMPILE-PREDICATE function
(let ((compenv (waql::add-qvar-compenv 'c
                 (waql::add-qvar-compenv 'b
                   (waql::add-qvar-compenv 'a
                     (waql::add-qvar-compenv 'u
                       (waql::add-qvar-compenv 'u1
                         (waql::empty-compenv))))))))
  (is (waql::compile-predicate '(waql::user= u u1) nil '(a b c)
                               compenv nil nil)
      '(when (waql::user= u u1)
         (iterate:in waql::outermost
           (collect-relation (tuple a b c))))))

(let ((compenv (waql::add-qvar-compenv 'c
                 (waql::add-qvar-compenv 'b
                   (waql::add-qvar-compenv 'a
                     (waql::empty-compenv))))))
  (is (waql::compile-predicate 'a nil '(a b c) compenv nil nil)
      '(when a
         (iterate:in waql::outermost
           (collect-relation (tuple a b c))))))


;;;
;;; test Compiler - Lisp form
;;;

(diag "test Compiler - Lisp form")

;;; test COMPILE-LISP-FORM function
(is (waql::compile-lisp-form '(lisp (= (waql::user-id u) 1)))
    '(= (waql::user-id u) 1))


;;;
;;; test Compiler - Function application
;;;

(diag "test Compiler - Function application")

;;; test COMPILE-FUNCTION function

(let ((compenv (waql::add-qvar-compenv 'u1
                 (waql::add-qvar-compenv 'u
                   (waql::empty-compenv)))))
  (is (waql::compile-function '(waql::user= u u1) compenv nil)
      '(waql::user= u u1)))

(is (waql::compile-function '(relation-count +r1+)
                            (waql::empty-compenv) nil)
    '(relation-count +r1+))

(let ((waql::*scoping-count* 1)
      (compenv (waql::add-letvar-compenv 'r '+r1+
                 (waql::add-letfun-compenv 'f '(x)
                                           '(query (a b) (<- (a b) x))
                   (waql::empty-compenv)))))
  (is (waql::compile-function '(f r) compenv nil)
      '(iterate:iter waql::outermost
         (for-tuple (%F2.a %F2.b) in-relation +r1+)
         (iterate:in waql::outermost
           (collect-relation (tuple %F2.a %F2.b))))))

(let ((compenv (waql::add-letvar-compenv 'r '+r1+
                 (waql::add-letfun-compenv 'f '(x) '(query (a b) (<- (a b) r))
                   (waql::empty-compenv)))))
  (is-error (waql::compile-function '(f r) compenv nil) simple-error))

(let ((compenv (waql::add-letfun-compenv 'f '(x) '(query (a b) (<- (a b) x))
                 (waql::empty-compenv))))
  (is-error (waql::compile-function '(f 1 2) compenv nil) simple-error))


;;;
;;; test Compiler - Compiling environment
;;;

(diag "test Compiler - Compiling environment")

;;; test EMPTY-COMPENV function
(is (waql::%compenv-elements (waql::empty-compenv)) nil)

;;; test ADD-QVAR-COMPENV function
(is (waql::lookup-compenv 'a
      (waql::add-qvar-compenv 'a
        (waql::empty-compenv)))
    :qvar)

;;; test ADD-ARGVAR-COMPENV function
(is (waql::lookup-compenv 'a
      (waql::add-argvar-compenv 'a 1
        (waql::empty-compenv)))
    '(:argvar 1))

;;; test ADD-LETVAR-COMPENV function
(destructuring-bind (keyword expr compenv)
    (waql::lookup-compenv 'a
      (waql::add-letvar-compenv 'a '(query (a b) (<- (a b) r))
        (waql::add-qvar-compenv 'a
          (waql::empty-compenv))))
  (is (list keyword expr)
      '(:letvar (query (a b) (<- (a b) r))))
  (is (waql::%compenv-elements compenv)
      '((a . :qvar))))

;;; test ADD-LETFUN-COMPENV function
(destructuring-bind (keyword args expr compenv)
    (waql::lookup-compenv 'a
      (waql::add-letfun-compenv 'a '(i) '(query (a b) (<- (a b) r)
                                                      (= a i))
        (waql::add-qvar-compenv 'a
          (waql::empty-compenv))))
  (is (list keyword args expr)
      '(:letfun (i) (query (a b) (<- (a b) r)
                                   (= a i))))
  (is (waql::%compenv-elements compenv)
      '((a . :qvar))))

;;; test PRINT-COMPENV function


;;;
;;; test Syntax - Literal
;;;

(diag "test Syntax - Literal")

;;; test LITERAL-P function
(is (waql::literal-p 1) t)
(is (waql::literal-p "foo") t)
(is (waql::literal-p '(time "2013/1/1" "10:10:35")) t)
(is (null (waql::literal-p 'a)) t)

;;; test TIME-LITERAL-DATE function
(is (waql::time-literal-date '(time "2013/1/1" "10:10:35")) "2013/1/1")
(is-error (waql::time-literal-date '(time foo "10:10:35")) simple-error)
(is (waql::time-literal-date '(time "2013/1/1" foo)) "2013/1/1")
(is-error (waql::time-literal-date '(time "2013/1/1")) simple-error)
(is-error (waql::time-literal-date '(time "2013/1/1" "10:10:35" foo))
          simple-error)

;;; test TIME-LITERAL-TIME function
(is (waql::time-literal-time '(time "2013/1/1" "10:10:35")) "10:10:35")
(is (waql::time-literal-time '(time foo "10:10:35")) "10:10:35")
(is-error (waql::time-literal-time '(time "2013/1/1" foo)) simple-error)
(is-error (waql::time-literal-time '(time "2013/1/1")) simple-error)
(is-error (waql::time-literal-time '(time "2013/1/1" "10:10:35" foo))
          simple-error)


;;;
;;; test Syntax - Symbol
;;;

(diag "test Syntax - Symbol")

;;; test SYMBOL-P function
(is (waql::symbol-p 'a) t)
(is (waql::symbol-p 1) nil)


;;;
;;; test Syntax - Let
;;;

(diag "test Syntax - Let")


;;;
;;; test Syntax - Query
;;;

(diag "test Syntax - Query")


;;;
;;; test Syntax - Query - Quantification
;;;

(diag "test Syntax - Query - Quantification")

;;; test MAKE-QUANTIFICATION constructor
(let ((q (waql::make-quantification '(a b c) 'foo)))
  (is (waql::quantification-vars q) '(a b c))
  (is (waql::quantification-relation q) 'foo))

;;; test QUANTIFICATION-P function
(is (waql::quantification-p '(<- (a b c) foo)) t)
(is (waql::quantification-p '(= 1 1)) nil)

;;; test QUANTIFICATION-VARS function
(is (waql::quantification-vars '(<- (a b c) foo)) '(a b c))
(is-error (waql::quantification-vars '(<- a foo)) simple-error)
(is-error (waql::quantification-vars '(= 1 1)) simple-error)
(is-error (waql::quantification-vars '(<- ((f i)) foo)) simple-error)

;;; test QUANTIFICATION-RELATION function
(is (waql::quantification-relation '(<- (a b c) foo)) 'foo)
(is-error (waql::quantification-relation '(= 1 1)) simple-error)


;;;
;;; test Function
;;;

(diag "test Function")


;;;
;;; test Type matching
;;;

(diag "test Type matching")

(is (waql::match-types-p '(:user :user) '(:user :user)) t)
(is (waql::match-types-p '(:user :event) '(:user :user)) nil)
(is (waql::match-types-p '((:relation :user)) '(:relation)) t)
(is (waql::match-types-p '((:relation :user)) '((:relation waql::_))) t)
(is (waql::match-types-p '((:relation :user :event)) '((:relation waql::_)))
    nil)
(is (waql::match-types-p '((:relation :user :event))
                         '((:relation :user :event)))
    t)


;;;
;;; test Type matching - Relation type pattern
;;;

(diag "test Type matching - Relation type pattern")

;;; test RELATION-TYPE-PATTERN-P function
(is (waql::relation-type-pattern-p :relation) t)
(is (waql::relation-type-pattern-p '(:relation waql::_ waql::_)) t)
(is (waql::relation-type-pattern-p '(:relation :user :event)) t)
(is (waql::relation-type-pattern-p :user) nil)
(is-error (waql::relation-type-pattern-p '(:relation)) simple-error)
(is-error (waql::relation-type-pattern-p '(:relation waql::_ :user))
          simple-error)

;;; test RELATION-TYPE-PATTERN-GENERAL-P function
(is (waql::relation-type-pattern-general-p :relation) t)
(is (waql::relation-type-pattern-general-p '(:relation waql::_)) nil)
(is (waql::relation-type-pattern-general-p '(:relation :user)) nil)
(is (waql::relation-type-pattern-general-p :user) nil)

;;; test RELATION-TYPE-PATTERN-WILDCARD-P function
(is (waql::relation-type-pattern-wildcard-p '(:relation waql::_ waql::_)) t)
(is (waql::relation-type-pattern-wildcard-p :relation) nil)
(is (waql::relation-type-pattern-wildcard-p '(:relation :user)) nil)
(is (waql::relation-type-pattern-wildcard-p :user) nil)
(is-error (waql::relation-type-pattern-wildcard-p '(:relation waql::_ :user))
          simple-error)

;;; test RELATION-TYPE-PATTERN-STRICT-P function
(is (waql::relation-type-pattern-strict-p '(:relation :user)) t)
(is (waql::relation-type-pattern-strict-p :relation) nil)
(is (waql::relation-type-pattern-strict-p '(:relation waql::_)) nil)
(is (waql::relation-type-pattern-strict-p :user) nil)
(is-error (waql::relation-type-pattern-strict-p '(:relation))
          simple-error)
(is-error (waql::relation-type-pattern-strict-p '(:relation :user waql::_))
          simple-error)

;;; test RELATION-TYPE-PATTERN-ATTRS function

;;; test RELATION-TYPE-PATTERN-DIM function


;;;
;;; test Type
;;;

(diag "test Type")


;;;
;;; test Type - Scalar types
;;;

(diag "test Type - Scalar types")

(ok (waql::scalar-type-p :bool))
(ok (waql::scalar-type-p :int))
(ok (waql::scalar-type-p :string))
(ok (waql::scalar-type-p :time))
(ok (waql::scalar-type-p :user))
(ok (waql::scalar-type-p :event))
(ok (waql::scalar-type-p :action))
(ok (waql::scalar-type-p :conversion))
(ok (null (waql::scalar-type-p 1)))


;;;
;;; test Type - Relation type
;;;

(diag "test Type - Relation type")

;;; test MAKE-RELATION-TYPE function
(ok (waql::make-relation-type '(:user :event)))
(is-error (waql::make-relation-type '((:relation :user :event)))
          simple-error)

;;; test RELATION-TYPE-P function
(ok (waql::relation-type-p '(:relation :user :event)))
(ok (null (waql::relation-type-p :user)))

;;; test RELATION-TYPE-ATTRS function
(is (waql::relation-type-attrs (waql::make-relation-type '(:user :event)))
    '(:user :event))
(is-error (waql::relation-type-attrs '(:relation)) simple-error)

;;; test RELATION-TYPE-DIM function


;;;
;;; test Type - Function type
;;;

(diag "test Type - Function type")

;;; test MAKE-FUNCTION-TYPE function
(ok (waql::make-function-type '(:int :int) :int))
(is-error (waql::make-function-type 1 :int) type-error)
(is-error (waql::make-function-type '(1) :int) simple-error)
(is-error (waql::make-function-type '(:int) 1) simple-error)

;;; test FUNCTION-TYPE-P function
(let ((function-type (waql::make-function-type '(:int :int) :int)))
  (is (waql::function-type-p function-type) t))

;;; test FUNCTION-TYPE-ARG-TYPES function
(let ((function-type (waql::make-function-type '(:int :int) :int)))
  (is (waql::function-type-arg-types function-type) '(:int :int)))

;;; test FUNCTION-TYPE-RETURN-TYPE function
(let ((function-type (waql::make-function-type '(:int :int) :int)))
  (is (waql::function-type-return-type function-type) :int))


;;;
;;; test Predefined relations
;;;

(diag "test Predefined relations")

;;; test DEFRELATION macro
(let ((waql::*predefined-relations* (waql::make-predefined-relations)))
  (ok (defrelation r (:int) (waql::empty-relation)))
  (is-error (defrelation r (:int) 1) type-error))


;;;
;;; test Parser - backtracking version
;;;

(diag "test Parser - backtracking version")

(is (parser-combinators:parse-string* (waql::expr?) "1") 1)
(is (parser-combinators:parse-string* (waql::expr?) "-1") -1)

(is (parser-combinators:parse-string* (waql::expr?) "x")
    'x)
(is (parser-combinators:parse-string* (waql::expr?) "+x+")
    '+x+)
(is (parser-combinators:parse-string* (waql::expr?) "user-id")
    'user-id)
(isnt (parser-combinators:parse-string* (waql::expr?) "0x")
      '0x)

(is (parser-combinators:parse-string* (waql::expr?) "let x := 1
                                                     in x")
    '(let (x 1) x))
(is (parser-combinators:parse-string* (waql::expr?) "let x := y
                                                     in x")
    '(let (x y) x))

(is (parser-combinators:parse-string* (waql::expr?) "let f i:int := i
                                                     in f 1")
    '(let (f ((i :int)) i) (f 1)))
(is (parser-combinators:parse-string* (waql::expr?) "let f i := i
                                                     in f 1")
    nil)
;; (is (parser-combinators:parse-string* (waql::expr?) "let f () := 1
;;                                                      in f ()")
;;     '(let (f () 1) (f)))

(is (parser-combinators:parse-string* (waql::expr?) "{ <a, b> | <a, b> <- R
                                                              , a = 1}")
    '(query (a b) (<- (a b) r)
                  (= a 1)))
(is (parser-combinators:parse-string* (waql::expr?) "{ <a> | <a, _> <- R }")
    '(query (a) (<- (a _) r)))

(is (parser-combinators:parse-string* (waql::expr?) "f i j")
    '(f i j))
(is (parser-combinators:parse-string* (waql::expr?) "f f i")
    '(f f i))
(is (parser-combinators:parse-string* (waql::expr?) "f (f i)")
    '(f (f i)))
(is (parser-combinators:parse-string* (waql::expr?) "let")
    nil)

(is (parser-combinators:parse-string* (waql::expr?) "1 = 1")
    '(= 1 1))
(is (parser-combinators:parse-string* (waql::expr?) "1 = f i = 1")
    '(= (= 1 (f i)) 1))

(is (parser-combinators:parse-string* (waql::expr?) "let x := 1
                                                     in -- x is bound to 1
                                                        x")
    '(let (x 1) x))

(is (parser-combinators:parse-string* (waql::expr-top?) "     1;")
    1)

(is (parser-combinators:parse-string* (waql::expr-top?) "     ;")
    nil)


;;;
;;; test Parser - non-backtracking version
;;;

(diag "test Parser - non-backtracking version")

(is (parser-combinators:parse-string* (waql::expr*) "1") 1)
(is (parser-combinators:parse-string* (waql::expr*) "-1") -1)

(is (parser-combinators:parse-string* (waql::expr*) "x")
    'x)
(is (parser-combinators:parse-string* (waql::expr*) "+x+")
    '+x+)
(is (parser-combinators:parse-string* (waql::expr*) "user-id")
    'user-id)
(isnt (parser-combinators:parse-string* (waql::expr*) "0x")
      '0x)

(is (parser-combinators:parse-string* (waql::expr*) "let x := 1
                                                     in x")
    '(let (x 1) x))
(is (parser-combinators:parse-string* (waql::expr*) "let x := y
                                                     in x")
    '(let (x y) x))

(is (parser-combinators:parse-string* (waql::expr*) "let f i:int := i
                                                     in f 1")
    '(let (f ((i :int)) i) (f 1)))
(is (parser-combinators:parse-string* (waql::expr*) "let f i := i
                                                     in f 1")
    nil)
;; (is (parser-combinators:parse-string* (waql::expr*) "let f () := 1
;;                                                      in f ()")
;;     '(let (f () 1) (f)))

(is (parser-combinators:parse-string* (waql::expr*) "{ <a, b> | <a, b> <- R
                                                              , a = 1}")
    '(query (a b) (<- (a b) r)
                  (= a 1)))
(is (parser-combinators:parse-string* (waql::expr*) "{ <a> | <a, _> <- R }")
    '(query (a) (<- (a _) r)))

(is (parser-combinators:parse-string* (waql::expr*) "f i j")
    '(f i j))
(is (parser-combinators:parse-string* (waql::expr*) "f f i")
    '(f f i))
(is (parser-combinators:parse-string* (waql::expr*) "f (f i)")
    '(f (f i)))
(is (parser-combinators:parse-string* (waql::expr*) "let")
    nil)

(is (parser-combinators:parse-string* (waql::expr*) "1 = 1")
    '(= 1 1))
(is (parser-combinators:parse-string* (waql::expr*) "1 = f i = 1")
    '(= (= 1 (f i)) 1))

(is (parser-combinators:parse-string* (waql::expr*) "let x := 1
                                                     in -- x is bound to 1
                                                        x")
    '(let (x 1) x))

(is (parser-combinators:parse-string* (waql::expr-top*) "     1;")
    1)

(is (parser-combinators:parse-string* (waql::expr-top*) "     ;")
    nil)


;;;
;;; test Utilities
;;:

(diag "test Utilities")

;;; test PERCENT-SYMBOL-P function
(is (waql::percent-symbol-p '%a) t)
(is (waql::percent-symbol-p 'a) nil)
(is (waql::percent-symbol-p '(f i)) nil)

;;; test SINGLE function
(is (waql::single '(1)) t)
(is (waql::single '(1 2)) nil)
(is (waql::single '()) nil)
(is (waql::single '(1 . 2)) nil)


(finalize)
