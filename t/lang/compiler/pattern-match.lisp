#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.pattern-match)

(plan nil)

;;
;; test PATTERN-MATCH-LITERAL function
;;

(diag "PATTERN-MATCH-LITERAL")

(is (pattern-match-literal 1) 1
    "basic case 1")

(is (pattern-match-literal "foo") "foo"
    "basic case 2")

(is (pattern-match-literal '(time "2013-1-1" "00:00:00"))
    '(time "2013-1-1" "00:00:00")
    "basic case 3")


;;
;; test PATTERN-MATCH-VARIABLE-REFERENCE function
;;

(diag "PATTERN-MATCH-VARIABLE-REFERENCE")

(let ((patenv (add-patenv 'x (empty-patenv))))
  (is (pattern-match-variable-reference 'x patenv)
      'x
      "basic case"))

(let ((patenv (empty-patenv)))
  (is-error (pattern-match-variable-reference '%x patenv) simple-error
            "EXPR which is a WAQL symbol beginning with a percent character"))

(is-error (pattern-match-variable-reference 'x 'foo) type-error
          "PATENV which is not a pattern matching environment")

(let ((patenv (empty-patenv)))
  (is-error (pattern-match-variable-reference 'x patenv) simple-error
            "EXPR which is unbound in PATENV"))


;;
;; test PATTERN-MATCH-LET function
;;

(diag "PATTERN-MATCH-LET")

(let ((patenv (add-patenv 'r (empty-patenv))))
  (is (pattern-match-let '(let (x 1)
                            (query (x) (<- (x) r))) patenv)
      '(let (x 1)
         (query (x) (<- (%x1) r)
                        (= x %x1)))
      "basic case 1")
  (is (pattern-match-let '(let (f ((i :int)) i)
                            (query ((f 1)) (<- (f) r))) patenv)
      '(let (f ((i :int)) i)
         (query ((f 1)) (<- (%f1) r)
                        (= f %f1)))
      "basic case 2"))

(let ((patenv (empty-patenv)))
  (is-error (pattern-match-let '(let (%x 1) %x) patenv) simple-error
            "EXPR whose variable part is a WAQL symbol beginning with a percent character 1")
  (is-error (pattern-match-let '(let (%f ((i :int)) i)
                                  (%f 1)) patenv)
            simple-error
            "EXPR whose variable part is a WAQL symbol beginning with a percent character 2"))

(let ((patenv (empty-patenv)))
  (is-error (pattern-match-let '(let (f ((%i :int)) %i)
                                  (f 1)) patenv)
            simple-error
            "EXPR in which any variables of argument part are WAQL symbols beginning with a percent character"))


;;
;; test PATTERN-MATCH-QUERY function
;;

(diag "PATTERN-MATCH-QUERY")

(let ((*underscore-count* 1)
      (patenv (add-patenv 'r
                (add-patenv 'x (empty-patenv)))))
  (is (pattern-match-query '(query (x) (<- (x y _ _) r)) patenv)
      '(query (x) (<- (%x1 y %_1 %_2) r)
                  (= x %x1))
      "basic case 1"))

(let ((*underscore-count* 1)
      (patenv (add-patenv 'r
                (empty-patenv))))
  (is (pattern-match-query '(query (x) (<- (x _) r)
                                       (<- (x _) r)) patenv)
      '(query (x) (<- (x %_1) r)
                  (<- (%x1 %_2) r)
                  (= x %x1))
      "basic case 2"))

(let ((patenv (empty-patenv)))
  (is-error (pattern-match-query '(query (%x) (<- (%x) r)) patenv)
            simple-error
            "EXPR whose var-list part contains any WAQL symbols beginning with a percent character"))

(let ((patenv (empty-patenv)))
  (is-error (pattern-match-query '(query (x) (<- (x x) r)) patenv)
            simple-error
            "EXPR whose var-list part contains duplicated WAQL symbols"))


;;
;; test PATTERN-MATCH-LISP-FORM function
;;

(diag "PATTERN-MATCH-LISP-FORM")

(let ((patenv (empty-patenv)))
  (is (pattern-match-lisp-form '(lisp foo :int))
      '(lisp foo :int)
      "basic case"))


;;
;; test PATTERN-MATCH-FUNCTION function
;;

(diag "PATTERN-MATCH-FUNCTION")

(let ((patenv (add-patenv 'r
                (add-patenv 'x (empty-patenv)))))
  (is (pattern-match-function '(foo (query (x) (<- (x) r)))
                              patenv)
      '(foo (query (x) (<- (%x1) r)
                       (= x %x1)))
      "basic case"))


;;
;; test RUN-PATTERN-MATCHER function
;;

(diag "RUN-PATTERN-MATCHER")

(let ((*underscore-count* 1)
      (patenv (add-patenv 'x (empty-patenv))))
  (multiple-value-bind (vars patenv1 preds)
      (run-pattern-matcher '(x y _ _) patenv)
    (is vars '(%x1 y %_1 %_2)
        "basic case 1")
    (is (lookup-patenv 'x patenv1) '(x . 2)
        "basic case 2")
    (is preds '((= x %x1))
        "basic case 3")))

(let ((patenv (empty-patenv)))
  (is-error (run-pattern-matcher 'foo patenv) type-error
            "VAR-LIST which is not a list of WAQL symbols"))

(is-error (run-pattern-matcher '(x y _ _) 'foo) type-error
          "PATENV which is not a pattern matching environment")



(finalize)
