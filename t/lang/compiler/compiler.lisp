#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler)

(plan nil)

(clear-defrelation)

(defrelation +r1+ (:int :int)
  (empty-relation))


;;
;; test COMPILE-WAQL function
;;

(diag "COMPILE-WAQL")

(is (compile-waql
      '(query (a2 c) (<- (a b) +r1+)
                     (<- (a2 c) (query (a1 c) (<- (a1 c) +r1+)
                                              (= a a1)))))
    '(iterate:iter outermost
       (for-tuple (a b) in-relation +r1+)
         (iterate:iter (for-tuple (a2 c) in-relation
                         (iterate:iter outermost
                           (for-tuple (a1 c) in-relation +r1+)
                           (when (= a a1)
                             (iterate:in outermost
                               (collect-relation (tuple a1 c))))))
                       (iterate:in outermost
                         (collect-relation (tuple a2 c)))))
    "recursive query")

(is (compile-waql
      '(let (u 1)
         (query (u e) (<- (u e)
           (query (u2 e) (<- (u2 e)
             (query (u3 e) (<- (u3 e) +r1+))))))))
    '(iterate:iter outermost
       (for-tuple (%u1 e)
         in-relation (iterate:iter outermost
                       (for-tuple (u2 e)
                         in-relation
                           (iterate:iter outermost
                             (for-tuple (u3 e) in-relation +r1+
                               using (list (list 1 nil)))
                             (iterate:in outermost
                               (collect-relation (tuple u3 e))))
                         using (list (list 1 nil)))
                       (iterate:in outermost
                         (collect-relation (tuple u2 e))))
         using (list (list 1 nil)))
       (when (= 1 %u1)
         (iterate:in outermost
           (collect-relation (tuple 1 e)))))
    "query in binder relation of quantification
  deriving lookup-keys")

(let ((*scoping-count* 1))
  (is (compile-waql
        '(let (+r2+ (query (u2 e) (<- (u2 e) +r1+)))
           (let (u 1)
             (query (u e) (<- (u e) +r2+)))))
      '(iterate:iter outermost
         (for-tuple (%u1 e)
           in-relation (iterate:iter outermost
                         (for-tuple (%+r2+2.u2 %+r2+2.e)
                           in-relation +r1+
                           using (list (list 1 nil)))
                         (iterate:in outermost
                           (collect-relation
                             (tuple %+r2+2.u2 %+r2+2.e))))
           using (list (list 1 nil)))
         (when (= 1 %u1)
           (iterate:in outermost
             (collect-relation (tuple 1 e)))))
      "symbol in binder relation of quantification
  deriving lookup-keys"))

(let ((*scoping-count* 1))
  (is (compile-waql
        '(let (f ((i :int)) (query (u2 e) (<- (u2 e) +r1+)))
           (let (u 1)
             (query (u e) (<- (u e) (f 1))))))
      '(iterate:iter outermost
         (for-tuple (%u1 e)
           in-relation (iterate:iter outermost
                         (for-tuple (%f2.u2 %f2.e) in-relation +r1+)
                         (iterate:in outermost
                           (collect-relation (tuple %f2.u2 %f2.e))))
           using (list (list 1 nil)))
         (when (= 1 %u1)
           (iterate:in outermost
             (collect-relation (tuple 1 e)))))
      "let-binded function application in binder relation of quantification
  not deriving lookup-keys"))

(is (compile-waql
      '(let (u 1)
         (query (u e) (<- (u e) (let (x 1)
                                  (query (u1 e) (<- (u1 e) +r1+)))))))
    '(iterate:iter outermost
       (for-tuple (%u1 e)
         in-relation (iterate:iter outermost
                       (for-tuple (u1 e) in-relation +r1+)
                       (iterate:in outermost
                         (collect-relation (tuple u1 e))))
         using (list (list 1 nil)))
       (when (= 1 %u1)
         (iterate:in outermost
           (collect-relation (tuple 1 e)))))
    "let variable binding in binder relation of quantification
  not deriving lookup-keys")

(is (compile-waql
      '(let (u 1)
         (query (u e) (<- (u e) (query ((let (x 1) x)
                                        e)
                                       (<- (u1 e) +r1+))))))
    '(iterate:iter outermost
       (for-tuple (%u1 e)
         in-relation (iterate:iter outermost
                       (for-tuple (u1 e) in-relation +r1+)
                       (iterate:in outermost
                         (collect-relation (tuple 1 e))))
         using (list (list 1 nil)))
       (when (= 1 %u1)
         (iterate:in outermost
           (collect-relation (tuple 1 e)))))
    "let variable binding in expressions of sub query
  not deriving lookup-keys")

(is (compile-waql
      '(let (u 1)
         (query (u e) (<- (u e) (query (u1 e1) (<- (u1 e1) +r1+)
                                               (<- (u1 e2) +r1+))))))
    '(iterate:iter outermost
       (for-tuple (%u1 e)
         in-relation (iterate:iter outermost
                       (for-tuple (u1 e1)
                         in-relation +r1+
                         using (list (list 1 nil)))
                       (iterate:iter
                         (for-tuple (%u11 e2)
                           in-relation +r1+
                           using (list (list u1 nil)))
                         (when (= u1 %u11)
                           (iterate:in outermost
                             (collect-relation (tuple u1 e1))))))
         using (list (list 1 nil)))
       (when (= 1 %u1)
         (iterate:in outermost
           (collect-relation (tuple 1 e)))))
    "case that sub query has several quantifications
  deriving lookup-keys to each quantification")

(is (compile-waql nil) nil
    "argument is nil")


(finalize)
