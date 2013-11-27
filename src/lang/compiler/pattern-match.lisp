#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.pattern-match)

;;
;;  Syntax:
;;
;;    PATTERN-MATCH-EXPRESSION-TOP expr => new-expr
;;
;;  Arguments:
;;
;;    expr --- a WAQL expression.
;;    new-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun pattern-match-expression-top (expr)
  (let ((patenv (reduce #'(lambda (patenv item)
                            (destructuring-bind (var . type) item
                              (declare (ignore type))
                              (add-patenv var patenv)))
                        (predefined-relations)
                        :initial-value (empty-patenv))))
    (pattern-match-expression expr patenv)))

;;
;;  Syntax:
;;
;;    PATTERN-MATCH-EXPRESSION expr patenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    patnev --- a pattern matching environment.
;;    new-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun pattern-match-expression (expr patenv)
  (cond
    ((literal-p expr) (pattern-match-literal expr))
    ((variable-reference-p expr)
     (pattern-match-variable-reference expr patenv))
    ((let-p expr) (pattern-match-let expr patenv))
    ((query-p expr) (pattern-match-query expr patenv))
    ((lisp-form-p expr) (pattern-match-lisp-form expr))
    ((function-p expr) (pattern-match-function expr patenv))
    (t (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    PATTERN-MATCH-LITERAL expr => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    new-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun pattern-match-literal (expr)
  expr)

;;
;;  Syntax:
;;
;;    PATTERN-MATCH-VARIABLE-REFERENCE expr patenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    patenv --- a pattern matching environment.
;;    new-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a WAQL symbol
;;    beginning with a percent character.
;;
;;    Signals an error of type type-error if PATENV is not a pattern
;;    matching environment.
;;
;;    Signals an error of type simple-error if EXPR is unbound in PATENV.
;;
(defun pattern-match-variable-reference (expr patenv)
  (unless (not (percent-symbol-p expr))
    (error "The value ~S, beginning with a percent character, is reserved."
           expr))
  (unless (lookup-patenv expr patenv)
    (error "The variable ~S is unbound." expr))
  expr)

;;
;;  Syntax:
;;
;;    PATTERN-MATCH-LET expr patenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    patenv --- a pattern matching environment.
;;    new-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if a variable part of EXPR
;;    is a WAQL symbol beginning with a percent character.
;;
;;    Signals an error of type simple-error if any variables of argument
;;    part of EXPR are WAQL symbols beginning with a percent character.
;;
(defun pattern-match-let (expr patenv)
  (cond
    ((let-variable-p expr) (pattern-match-let-var expr patenv))
    ((let-function-p expr) (pattern-match-let-fun expr patenv))
    (t (error "The value ~S is an invalid expression." expr))))

(defun pattern-match-let-var (expr patenv)
  (let ((var (let-var expr)))
    (unless (not (percent-symbol-p var))
      (error "The value ~S, beginning with a percent character, is reserved." var))
    (let ((lvar (let-var expr))
          (llocal (let-local-expr expr))
          (lbody (let-body-expr expr)))
      (let ((patenv1 (add-patenv lvar patenv)))
        (let ((llocal1 (pattern-match-expression llocal patenv))
              (lbody1 (pattern-match-expression lbody patenv1)))
          (make-let-variable lvar llocal1 lbody1))))))

(defun pattern-match-let-fun (expr patenv)
  (let ((var (let-var expr)))
    (unless (not (percent-symbol-p var))
      (error "The value ~S, beginning with a percent character, is reserved." var)))
  (dolist (var (let-arg-vars expr))
    (unless (not (percent-symbol-p var))
      (error "The value ~S, beginning with a percent character, is reserved." var)))
  (let ((lvar (let-var expr))
        (largs (let-args expr))
        (larg-vars (let-arg-vars expr))
        (llocal (let-local-expr expr))
        (lbody (let-body-expr expr)))
    (let ((patenv-local (bulk-add-patenv larg-vars patenv))
          (patenv-body (add-patenv lvar patenv)))
      (let ((llocal1 (pattern-match-expression llocal patenv-local))
            (lbody1 (pattern-match-expression lbody patenv-body)))
        (make-let-function lvar largs llocal1 lbody1)))))

;;
;;  Syntax:
;;
;;    PATTERN-MATCH-QUERY expr patenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    patenv --- a pattern matching environment.
;;    new-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if var-list part of EXPR
;;    contains any WAQL symbols beginning with a percent character.
;;
;;    Signals an error of type simple-error if var-list part of EXPR
;;    contains duplicated WAQL symbols, except underscore notations.
;;
(defun pattern-match-query (expr patenv)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (multiple-value-bind (quals1 exprs1)
        (pattern-match-quals quals exprs patenv)
      (make-query exprs1 quals1))))

(defun pattern-match-quals (quals exprs patenv)
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (multiple-value-bind (qual1 rest1 exprs1)
            (pattern-match-qual qual rest exprs patenv)
          (values (cons qual1 rest1) exprs1)))
      (let ((exprs1 (pattern-match-exprs exprs patenv)))
        (values nil exprs1))))

(defun pattern-match-qual (qual rest exprs patenv)
  (if (quantification-p qual)
      (pattern-match-quantification qual rest exprs patenv)
      (pattern-match-predicate qual rest exprs patenv)))

(defun duplicated-vars-p (vars)
  (let ((vars1 (remove-if #'underscore-notation-p vars)))
    (not (equal vars1 (remove-duplicates vars1)))))

(defun pattern-match-quantification (qual rest exprs patenv)
  (dolist (var (quantification-vars qual))
    (unless (or (underscore-notation-p var)
                (not (percent-symbol-p var)))
        (error "The value ~S, beginning with a percent character, is reserved." var)))
  (let ((vars (quantification-vars qual)))
    (unless (not (duplicated-vars-p vars))
      (error "The values ~S are duplicated." vars)))
  (let ((vars (quantification-vars qual))
        (rel (quantification-relation qual)))
    ;; pattern matching on relation-expr part
    (let ((rel1 (pattern-match-expression rel patenv)))
      ;; pattern matching on the quantification
      (multiple-value-bind (vars1 patenv1 preds)
          (run-pattern-matcher vars patenv)
        (let ((qual1 (make-quantification vars1 rel1)))
          ;; pattern matching on REST and EXPRS
          (multiple-value-bind (rest1 exprs1)
              (pattern-match-quals rest exprs patenv1)
            (values qual1 (append preds rest1) exprs1)))))))

(defun pattern-match-predicate (pred rest exprs patenv)
  (let ((pred1 (pattern-match-expression pred patenv)))
    (multiple-value-bind (rest1 exprs1)
        (pattern-match-quals rest exprs patenv)
      (values pred1 rest1 exprs1))))

(defun pattern-match-exprs (exprs patenv)
  (mapcar #'(lambda (expr)
              (pattern-match-expression expr patenv))
          exprs))

;;
;;  Syntax:
;;
;;    PATTERN-MATCH-LISP-FORM expr => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    new-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun pattern-match-lisp-form (expr)
  expr)

;;
;;  Syntax:
;;
;;    PATTERN-MATCH-FUNCTION expr patenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    patenv --- a pattern matching environment.
;;    new-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun pattern-match-function (expr patenv)
  (let ((operator (function-operator expr))
        (operands (function-operands expr)))
    (let ((operands1 (mapcar #'(lambda (operand)
                                 (pattern-match-expression operand patenv))
                             operands)))
      `(,operator ,@operands1))))

;;
;;  Syntax:
;;
;;    RUN-PATTERN-MATCHER var-list patenv => new-var-list, new-patenv, predicate-list
;;
;;  Arguments and Values:
;;
;;    var-list --- a list of WAQL symbols.
;;    patenv --- a pattern matching environment.
;;    new-var-list --- a list of WAQL symbols.
;;    new-patenv --- a pattern matching environment.
;;    predicate-list --- a list of WAQL expressions.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR-LIST is not a list of WAQL
;;    symbols or underscore notations.
;;
;;    Signals an error of type type-error if PATENV is not a pattern
;;    matching environment.
;;
(defstruct (pattern-matcher (:constructor %make-pattern-matcher))
  (vars :vars :read-only t)
  (patenv :patenv :read-only t)
  (predicates :predicates :read-only t))

(defun make-pattern-matcher (patenv)
  (%make-pattern-matcher :vars nil
                         :patenv patenv
                         :predicates nil))

(defmacro with-pattern-matcher (((vars patenv preds) matcher) &body form)
  (with-gensyms (vars1 patenv1 preds1)
    `(let ((,vars (pattern-matcher-vars ,matcher))
           (,patenv (pattern-matcher-patenv ,matcher))
           (,preds (pattern-matcher-predicates ,matcher)))
       (multiple-value-bind (,vars1 ,patenv1 ,preds1) ,@form
         (%make-pattern-matcher :vars ,vars1
                                :patenv ,patenv1
                                :predicates ,preds1)))))

(defun pattern-matcher-match (matcher var)
  (unless (or (waql-symbol-p var)
              (underscore-notation-p var))
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (if (underscore-notation-p var)
      (pattern-matcher-match-underscore matcher var)
      (pattern-matcher-match-otherwise matcher var)))

(defvar *underscore-count* 1)

(defun pattern-matcher-match-underscore (matcher var)
  (with-pattern-matcher ((vars patenv preds) matcher)
    (let ((vars1 (cons (percent-symbol var *underscore-count*) vars)))
      (incf *underscore-count*)
      (values vars1 patenv preds))))

(defun pattern-matcher-match-otherwise (matcher var)
  (with-pattern-matcher ((vars patenv preds) matcher)
    (cl-pattern:match (lookup-patenv var patenv)
      ((_ . count)
       (let* ((var1 (percent-symbol var count))
              (vars1 (cons var1 vars))
              (patenv1 (inc-patenv var patenv))
              (preds1 (cons `(= ,var ,var1) preds)))
         (values vars1 patenv1 preds1)))
      (_
       (let ((vars1 (cons var vars))
             (patenv1 (add-patenv var patenv)))
         (values vars1 patenv1 preds))))))

(defun pattern-matcher-match-all (var-list matcher)
  (reduce #'pattern-matcher-match
          var-list :initial-value matcher))

(defun pattern-matcher-result (matcher)
  (values (reverse (pattern-matcher-vars matcher))
          (pattern-matcher-patenv matcher)
          (reverse (pattern-matcher-predicates matcher))))

(defun run-pattern-matcher (var-list patenv)
  (pattern-matcher-result
    (pattern-matcher-match-all var-list
      (make-pattern-matcher patenv))))
