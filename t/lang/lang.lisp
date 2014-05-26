#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang)

(defrelation +r1+ (:int :int)
  (relation-adjoin-all
    (list (tuple 1 1) (tuple 1 2) (tuple 2 3))
    (empty-relation)))

(plan nil)

;;
;; test WAQL macro
;;

(diag "WAQL macro")

(let ((result (waql "{ <u> | <u, ev> <- +r1+ };")))
  (is (relation-member result (tuple 1)) t
      "projection 1")
  (is (relation-count result) 2
      "projection 2"))

(let ((result (waql "{ <u, ev> | <u, ev> <- +r1+
                               , u = 1 };")))
  (is (relation-member result (tuple 1 1)) t
      "selection 1")
  (is (relation-member result (tuple 2 3)) nil
      "selection 2")
  (is (relation-count result) 2
      "selection 3"))

(let ((result (waql "{ <u1, ev1, u2, ev2> | <u1, ev1> <- +r1+
                                          , <u2, ev2> <- +r1+ };")))
  (is (relation-member result (tuple 1 1 2 3)) t
      "Cartesian product 1")
  (is (relation-count result) 9
      "Cartesian product 2"))

(let ((result (waql "{ <u1, ev1, ev2> | <u1, ev1> <- +r1+
                                      , <u2, ev2> <- +r1+
                                      , u1 = u2
                                      , ev1 < ev2 };")))
  (is (relation-member result (tuple 1 1 2)) t
      "natural join 1")
  (is (relation-count result) 1
      "natural join 2"))

(let ((result (waql "{ < u,
                         count { <ev1> | <u, ev1> <- +r1+ } >
                     | <u, ev> <- +r1+ };")))
  (is (relation-member result (tuple 1 2)) t
      "count aggregation 1")
  (is (relation-member result (tuple 2 1)) t
      "count aggregation 2")
  (is (relation-count result) 2
      "count aggregation 3"))

(let ((result (waql "{ <u> | <u, _> <- +r1+
                           , <u, _> <- +r1+ };")))
  (is (relation-member result (tuple 1)) t
      "underscore notation 1")
  (is (relation-member result (tuple 2)) t
      "underscore notation 2")
  (is (relation-count result) 2
      "underscore notation 3"))

(let ((result (waql "let x := { <u, e> | <u, e> <- +r1+ }
                     in let f u:int i:int := u = i
                        in { <u> | <u, e> <- x
                                 , f u 1 };")))
  (is (relation-member result (tuple 1)) t
      "let binding 1")
  (is (relation-count result) 1
      "let binding 2"))


;;
;; test EVAL-WAQL function
;;

(diag "EVAL-WAQL")

(let ((code "123"))
  (is (eval-waql code) 123
      "basic case"))


;;
;; test PRECOMPILE-WAQL function
;;

(diag "PRECOMPILE-WAQL")

(let ((thunk (precompile-waql "123")))
  (is (type-of thunk) 'function
      "basic case 1")
  (is (funcall thunk) 123
      "basic case 2"))

(let ((thunk (precompile-waql "lisp \"x\" int" 'x)))
  (is (funcall thunk 1) 1
      "basic case 3"))

(let ((thunk (precompile-waql "lisp \"(+ x y)\" int" 'x 'y)))
  (is (funcall thunk 1 2) 3
      "basic case 4"))


(finalize)
