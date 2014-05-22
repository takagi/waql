#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.syntax)

(plan nil)

;;
;; test LITERAL-P function
;;

(diag "LITERAL-P")

(is (literal-p 1) t
    "basic case 1")

(is (literal-p "foo") t
    "basic case 2")

(is (literal-p '(time "2013-1-1" "00:00:00")) t
    "basic case 3")

(is (literal-p 'foo) nil
    "basic case 4")


;;
;; test MAKE-TIME-LITERAL function
;;

(diag "MAKE-TIME-LITERAL")

(let ((time (make-time-literal "2013-1-1" "00:00:00")))
  (is (time-literal-date time) "2013-1-1"
      "basic case 1")
  (is (time-literal-time time) "00:00:00"
      "basic case 2"))

(is-error (make-time-literal 'foo "00:00:00") type-error
          "DATE-STRING which is not a waql-string")

(is-error (make-time-literal "2013-1-1" 'foo) type-error
          "TIME-STRING which is not a waql-string")


;;
;; test TIME-LITERAL-P function
;;

(diag "TIME-LITERAL-P")

(is (time-literal-p '(time "2013-1-1" "00:00:00")) t
    "basic case 1")

(is (time-literal-p '(time)) t
    "basic case 2")

(is (time-literal-p 'foo) nil
    "basic case 3")


;;
;; test TIME-LITERAL-DATE function
;;

(diag "TIME-LITERAL-DATE")

(is (time-literal-date '(time "2013-1-1" "00:00:00")) "2013-1-1"
    "basic case")

(is-error (time-literal-date '(time)) simple-error
          "EXPR which is malformed")

(is-error (time-literal-date 'foo) simple-error
          "EXPR which is not a WAQL expression of time literal")


;;
;; test TIME-LITERAL-DATE function
;;

(diag "TIME-LITERAL-DATE")

(is (time-literal-time '(time "2013-1-1" "00:00:00")) "00:00:00"
    "basic case")

(is-error (time-literal-time '(time)) simple-error
          "EXPR which is malformed")

(is-error (time-literal-time 'foo) simple-error
          "EXPR which is not a WAQL expression of time literal")


;;
;; test VARIABLE-REFERENCE-P function
;;

(diag "VARIABLE-REFERENCE-P")

(is (variable-reference-p 'x) t
    "basic case 1")

(is (variable-reference-p 1) nil
    "basic case 2")


;;
;; test MAKE-LET-VARIABLE function
;;

(diag "MAKE-LET-VARIABLE")

(let ((expr (make-let-variable 'x 1 'x)))
  (is (let-var expr) 'x
      "basic case 1")
  (is (let-local-expr expr) 1
      "basic case 2")
  (is (let-body-expr expr) 'x
      "basic case 3"))

(is-error (make-let-variable 1 1 'x) type-error
          "VAR which is not a WAQL symbol")


;;
;; test MAKE-LET-FUNCTION function
;;

(diag "MAKE-LET-FUNCTION")

(let ((expr (make-let-function 'f '((i :int) (j :int)) '(< i j) '(f 1 2))))
  (is (let-var expr) 'f
      "basic case 1")
  (is (let-args expr) '((i :int) (j :int))
      "basic case 2")
  (is (let-local-expr expr) '(< i j)
      "basic case 3")
  (is (let-body-expr expr) '(f 1 2)
      "basic case 4"))

(is-error (make-let-function 1 '((i :int) (j :int)) '(< i j) '(f 1 2))
          type-error
          "VAR which is not a WAQL symbol")

(is-error (make-let-function 'f 'foo '(< i j) '(f 1 2))
          type-error
          "ARG-LIST which is not a list of arguments 1")

(is-error (make-let-function 'f '(i (j :int)) '(< i j) '(f 1 2))
          simple-error
          "ARG-LIST which is not a list of arguments 2")


;;
;; test LET-P function
;;

(diag "LET-P")

(is (let-p '(let (x 1) x)) t
    "basic case 1")

(is (let-p '(let (f ((i :int) (j :int))
                   (< i j))
              (f 1 2))) t
    "basic case 2")

(is (let-p '(let)) t
    "basic case 3")

(is (let-p 'foo) nil
    "basic case 4")


;;
;; test LET-VARIABLE-P function
;;

(diag "LET-VARIABLE-P")

(is (let-variable-p '(let (x 1) x)) t
    "basic case 1")

(is (let-variable-p '(let (f ((i :int) (j :int))
                            (< i j))
                       (f 1 2)))
    nil
    "basic case 2")

(is (let-variable-p '(let)) nil
    "basic case 3")


;;
;; test LET-FUNCTION-P function
;;

(diag "LET-FUNCTION-P")

(is (let-function-p '(let (f ((i :int) (j :int))
                            (< i j))
                       (f 1 2)))
    t
    "basic case 1")

(is (let-function-p '(let (x 1) x)) nil
    "basic case 2")

(is (let-function-p '(let)) nil
    "basic case 3")


;;
;; test LET-VAR function
;;

(diag "LET-VAR")

(is (let-var '(let (x 1) x)) 'x
    "basic case 1")

(is (let-var '(let (f ((i :int) (j :int))
                     (< i j))
                (f 1 2))) 'f
    "basic case 2")

(is-error (let-var '(let)) simple-error
          "EXPR which is malformed")

(is-error (let-var 'foo) simple-error
          "EXPR which is not a WAQL expression of let binding")


;;
;; test LET-ARGS function
;;

(diag "LET-ARGS")

(is (let-args '(let (f ((i :int) (j :int))
                      (< i j))
                 (f 1 2)))
    '((i :int) (j :int))
    "basic case")

(is-error (let-args '(let (x 1) x)) simple-error
          "EXPR which is a WAQL expression of let binding for a variable")

(is-error (let-args '(let)) simple-error
          "EXPR which is malformed")

(is-error (let-args 'foo) simple-error
          "EXPR which is not a WAQL expression of let binding")


;;
;; test LET-ARG-VARS function
;;

(diag "LET-ARG-VARS")

(is (let-arg-vars '(let (f ((i :int) (j :int))
                          (< i j))
                     (f 1 2)))
    '(i j)
    "basic case")


;;
;; test LET-ARG-TYPES function
;;

(diag "LET-ARG-TYPES")

(is (let-arg-types '(let (f ((i :int) (j :int))
                           (< i j))
                      (f 1 2)))
    '(:int :int)
    "basic case")


;;
;; test LET-LOCAL-EXPR function
;;

(diag "LET-LOCAL-EXPR")

(is (let-local-expr '(let (x 1) x)) 1
    "basic case 1")

(is (let-local-expr '(let (f ((i :int) (j :int))
                            (< i j))
                       (f 1 2)))
    '(< i j)
    "basic case 2")

(is-error (let-local-expr '(let)) simple-error
          "EXPR which is malformed")

(is-error (let-local-expr 'foo) simple-error
          "EXPR which is not a WAQL expression of let binding")


;;
;; test LET-BODY-EXPR function
;;

(diag "LET-BODY-EXPR")

(is (let-body-expr '(let (x 1) x)) 'x
    "basic case 1")

(is (let-body-expr '(let (f ((i :int) (j :int))
                           (< i j))
                      (f 1 2)))
    '(f 1 2)
    "basic case 2")

(is-error (let-body-expr '(let)) simple-error
          "EXPR which is malformed")

(is-error (let-body-expr 'foo) simple-error
          "EXPR which is not a WAQL expression of let binding")


;;
;; test MAKE-ARGUMENT function
;;

(diag "MAKE-ARGUMENT")

(let ((arg (make-argument 'x :int)))
  (is (argument-var arg) 'x
      "basic case 1")
  (is (argument-type arg) :int
      "basic case 2"))

(is-error (make-argument 1 :int) type-error
          "VAR which is not a WAQL symbol")

(is-error (make-argument 'x :foo) type-error
          "TYPE which is not a WAQL type")


;;
;; test ARGUMENT-P function
;;

(diag "ARGUMENT-P")

(is (argument-p '(x :int)) t
    "basic case 1")

(is (argument-p '(x)) nil
    "basic case 2")

(is (argument-p 'foo) nil
    "basic case 3")


;;
;; test ARGUMENT-VAR function
;;

(diag "ARGUMENT-VAR")

(is (argument-var '(i :int)) 'i
    "basic case")

(is-error (argument-var '(i)) simple-error
          "ARG which is not an argument 1")

(is-error (argument-var 'foo) simple-error
          "ARG which is not an argument 2")


;;
;; test ARGUMENT-TYPE function
;;

(diag "ARGUMENT-TYPE")

(is (argument-type '(i :int)) :int
    "basic case")

(is-error (argument-type '(i)) simple-error
          "ARG which is not an argument 1")

(is-error (argument-type 'foo) simple-error
          "ARG which is not an argument 2")


;;
;; test MAKE-QUERY function
;;

(diag "MAKE-QUERY")

(let ((expr (make-query '(x y) '((<- (x y) r) (< x y)))))
  (is (query-exprs expr) '(x y)
      "basic case 1")
  (is (query-quals expr) '((<- (x y) r) (< x y))
      "basic case 2"))

(is-error (make-query 'foo '((<- (x y) r) (< x y))) type-error
          "EXPR-LIST which is not a list")

(is-error (make-query '(x y) 'foo) type-error
          "QUAL-LIST which is not a list")


;;
;; test QUERY-P function
;;

(diag "QUERY-P")

(is (query-p '(query (x y) (<- (x y) r) (< x y))) t
    "basic case 1")

(is (query-p '(query)) t
    "basic case 2")

(is (query-p 'foo) nil
    "basic case 3")


;;
;; test QUERY-EXPRS function
;;

(diag "QUERY-EXPRS")

(is (query-exprs '(query (x y) (<- (x y) r) (< x y))) '(x y)
    "basic case")

(is-error (query-exprs '(query)) simple-error
          "EXPR which is malformed")

(is-error (query-exprs 'foo) simple-error
          "EXPR which is not a WAQL expression of query")


;;
;; test QUERY-QUALS function
;;

(diag "QUERY-QUALS")

(is (query-quals '(query (x y) (<- (x y) r) (< x y)))
    '((<- (x y) r) (< x y))
    "basic case")

(is-error (query-quals '(query)) simple-error
          "EXPR which is malformed")

(is-error (query-quals 'foo) simple-error
          "EXPR which is not a WAQL expression of query")


;;
;; test MAKE-QUANTIFICATION function
;;

(diag "MAKE-QUANTIFICATION")

(let ((quant (make-quantification '(x y _ 1) 'r)))
  (is (quantification-vars quant) '(x y _ 1)
      "basic case 1")
  (is (quantification-relation quant) 'r
      "basic case 2"))

(is-error (make-quantification 'foo 'r) type-error
          "VAR-LIST which is not a list of WAQL symbols 1")


;;
;; test QUANTIFICATION-P function
;;

(diag "QUANTIFICATION-P")

(is (quantification-p '(<- (x y) r)) t
    "basic case 1")

(is (quantification-p '(<-)) t
    "basic case 2")

(is (quantification-p 'foo) nil
    "basic case 3")


;;
;; test QUANTIFICATION-VARS function
;;

(diag "QUANTIFICATION-VARS")

(is (quantification-vars '(<- (x y) r)) '(x y)
    "basic case")

(is-error (quantification-vars '(<-)) simple-error
          "QUANTIFICATION which is malformed")

(is-error (quantification-vars 'foo) simple-error
          "QUANTIFICATION which is not a quantification")


;;
;; test QUANTIFICATION-RELATION function
;;

(diag "QUANTIFICATION-RELATION")

(is (quantification-relation '(<- (x y) r)) 'r
    "basic case")

(is-error (quantification-relation '(<-)) simple-error
          "QUANTIFICATION which is malformed")

(is-error (quantification-relation 'foo) simple-error
          "QUANTIFICATION which is not a quantification")


;;
;; test MAKE-LISP-FORM function
;;

(diag "MAKE-LISP-FORM")

(let ((expr (make-lisp-form '(+ 1 2) :int)))
  (is (lisp-form expr) '(+ 1 2)
      "basic case 1")
  (is (lisp-form-type expr) :int
      "basic case 2"))

(is-error (make-lisp-form '(+ 1 2) :foo) type-error
          "TYPE which is not a WAQL type")


;;
;; test LISP-FORM-P function
;;

(diag "LISP-FORM-P")

(is (lisp-form-p '(lisp (+ 1 2) :int)) t
    "basic case 1")

(is (lisp-form-p '(lisp)) t
    "basic case 2")

(is (lisp-form-p 'foo) nil
    "basic case 3")


;;
;; test LISP-FORM function
;;

(diag "LISP-FORM")

(is (lisp-form '(lisp (+ 1 2) :int)) '(+ 1 2)
    "basic case")

(is-error (lisp-form '(lisp)) simple-error
          "EXPR which is malformed")

(is-error (lisp-form 'foo) simple-error
          "EXPR which is not a WAQL expression of lisp form")


;;
;; test LISP-FORM-TYPE function
;;

(diag "LISP-FORM-TYPE")

(is (lisp-form-type '(lisp (+ 1 2) :int)) :int
    "basic case")

(is-error (lisp-form-type '(lisp)) simple-error
          "EXPR which is malformed")

(is-error (lisp-form-type 'foo) simple-error
          "EXPR which is not a WAQL expression of lisp form")


;;
;; test MAKE-FUNCTION function
;;

(diag "MAKE-FUNCTION")

(let ((expr (make-function 'f '(1 2))))
  (is (function-operator expr) 'f
      "basic case 1")
  (is (function-operands expr) '(1 2)
      "basic case 2"))


;;
;; test FUNCTION-P function
;;

(diag "FUNCTION-P")

(is (function-p '(f 1 2)) t
    "basic case 1")

(is (function-p 'foo) nil
    "basic case 2")


;;
;; test FUNCTION-OPERATOR function
;;

(diag "FUNCTION-OPERATOR")

(is (function-operator '(f 1 2)) 'f
    "basic case")

(is-error (function-operator 'foo) simple-error
          "EXPR which is not a WAQL expression of function application")


;;
;; test FUNCTION-OPERANDS function
;;

(diag "FUNCTION-OPERANDS")

(is (function-operands '(f 1 2)) '(1 2)
    "basic case")

(is-error (function-operands 'foo) simple-error
          "EXPR which is not a WAQL expression of function application")



(finalize)
