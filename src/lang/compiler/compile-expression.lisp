#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.compile-expression)

;;
;;  Syntax:
;;
;;    COMPILE-EXPRESSION-TOP expr => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun compile-expression-top (expr)
  (let ((compenv (reduce #'(lambda (compenv item)
                             (destructuring-bind (var type _) item
                               (declare (ignore _))
                               (add-argvar-compenv var type var
                                                   compenv)))
                         (predefined-relations)
                         :initial-value (empty-compenv))))
    (compile-expression expr compenv nil nil)))

;;
;;  Syntax:
;;
;;    COMPILE-EXPRESSION expr compenv scope lookup-keys => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    compenv --- a compiling environment.
;;    scope --- a WAQL symbol.
;;    lookup-keys --- a list of lookup-keys.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun compile-expression (expr compenv scope lookup-keys)
  (cond
    ((literal-p expr) (compile-literal expr))
    ((variable-reference-p expr)
     (compile-variable-reference expr compenv scope lookup-keys))
    ((let-p expr) (compile-let expr compenv scope))
    ((query-p expr) (compile-query expr compenv scope lookup-keys))
    ((lisp-form-p expr) (compile-lisp-form expr))
    ((function-p expr) (compile-function expr compenv scope))
    (t (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    COMPILE-LITERAL expr => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun compile-literal (expr)
  (cond
    ((int-literal-p expr) expr)
    ((string-literal-p expr) expr)
    ((time-literal-p expr) (compile-literal-time expr))
    (t (error "The value ~S is an invalid expression." expr))))

(defun compile-literal-time (expr)
  (let ((date (time-literal-date expr))
        (time (time-literal-time expr)))
    (let ((timestring (format nil "~AT~A" date time)))
      `(parse-timestring ,timestring))))

;;
;;  Syntax:
;;
;;    COMPILE-VARIABLE-REFERENCE expr compenv scope lookup-keys => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    compenv --- a compiling environment.
;;    scope --- a WAQL symbol.
;;    lookup-keys --- a list of lookup-keys.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if COMPENV is not a compiling
;;    environment.
;;
;;    Signals an error of type simple-error if EXPR is unbound in COMPENV.
;;
;;    Signals an error of type simple-error if EXPR is bound to a function
;;    in COMPENV.
;;
(defun compile-variable-reference (expr compenv scope lookup-keys)
  (cl-pattern:match (lookup-compenv expr compenv)
    ((:qvar _) (scoped-symbol expr scope))
    ((:argvar _ expr1) expr1)
    ((:letvar _ expr1 compenv1)
     (let ((scope1 (scoping-symbol expr)))
       (compile-expression expr1 compenv1 scope1 lookup-keys)))
    ((:letfun . _)
     (error "The variable ~S is bound to a function." expr))
    (_ (error "The variable ~S is unbound." expr))))

;;
;;  Syntax:
;;
;;    COMPILE-LET expr compenv scope => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    compenv --- a compiling environment.
;;    scope --- a WAQL symbol.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun compile-let (expr compenv scope)
  (cond
    ((let-variable-p expr) (compile-let-var expr compenv scope))
    ((let-function-p expr) (compile-let-fun expr compenv scope))
    (t (error "The value ~S is an invalid expression." expr))))

(defun compile-let-var (expr compenv scope)
  (let ((lvar (let-var expr))
        (llocal (let-local-expr expr))
        (lbody (let-body-expr expr)))
    (let* ((typenv (compenv->typenv compenv))
           (lreturn-type (type-of-expression llocal typenv)))
      (let ((compenv1 (add-letvar-compenv lvar lreturn-type llocal
                                          compenv)))
        (compile-expression lbody compenv1 scope nil)))))

(defun compile-let-fun (expr compenv scope)
  (let ((lvar (let-var expr))
        (largs (let-args expr))
        (llocal (let-local-expr expr))
        (lbody (let-body-expr expr)))
    (let* ((typenv (compenv->typenv compenv))
           (ltypenv (add-argvars-typenv largs typenv))
           (lreturn-type (type-of-expression llocal ltypenv)))
      (let ((compenv1 (add-letfun-compenv lvar largs lreturn-type llocal
                                          compenv)))
        (compile-expression lbody compenv1 scope nil)))))

;;
;;  Syntax:
;;
;;    COMPILE-QUERY expr compenv scope lookup-keys => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    compenv --- a compiling environment.
;;    scope --- a WAQL symbol.
;;    lookup-keys --- a list of lookup-keys.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if the first qualification of
;;    of EXPR is not a quantification.
;;
(defun compile-query (expr compenv scope lookup-keys)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (compile-query-quals quals exprs compenv scope lookup-keys t)))

(defun compile-query-quals (quals exprs compenv scope lookup-keys
                            outermost)
  (when outermost
    (unless (quantification-p (car quals))
      (error "The first qualification ~S is not a quantification."
             (car quals))))
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (compile-query-qual qual rest exprs compenv scope lookup-keys
                            outermost))
      (compile-query-exprs exprs compenv scope)))

(defun compile-query-qual (qual rest exprs compenv scope lookup-keys
                           outermost)
  (if (quantification-p qual)
      (compile-quantification qual rest exprs compenv scope
                                    lookup-keys outermost)
      (compile-predicate qual rest exprs compenv scope lookup-keys)))

(defun compile-quantification (qual rest exprs compenv scope lookup-keys
                               outermost)
  (let ((vars (quantification-vars qual))
        (rel (quantification-relation qual)))
    (let* ((typenv (compenv->typenv compenv))
           (rel-type-attrs (relation-type-attributes
                             (type-of-expression rel typenv)))
           (compenv1 (add-qvars-compenv vars rel-type-attrs compenv)))
      (let* ((lookup-keys1 (compute-lookup-keys vars exprs compenv scope
                                                lookup-keys))
             (compiled-lookup-keys1 (compile-lookup-keys lookup-keys1)))
        (let ((vars1 (mapcar #'(lambda (var)
                                 (scoped-symbol var scope))
                             vars))
              (rel1 (compile-expression rel compenv scope lookup-keys1))
              (quals1 (compile-query-quals rest exprs compenv1 scope
                                           lookup-keys nil)))
          `(iterate:iter ,@(if outermost '(outermost))
             (for-tuple ,vars1 in-relation ,rel1
               ,@(if lookup-keys1 `(using ,compiled-lookup-keys1)))
             ,quals1))))))

(defun compile-predicate (pred rest exprs compenv scope lookup-keys)
  `(when ,(compile-expression pred compenv scope nil)
     ,(compile-query-quals rest exprs compenv scope lookup-keys nil)))

(defun compile-query-exprs (exprs compenv scope)
  (let ((exprs1 (mapcar #'(lambda (expr)
                            (compile-expression expr compenv scope nil))
                        exprs)))
    `(iterate:in outermost
       (collect-relation (tuple ,@exprs1)))))

;;
;;  Syntax:
;;
;;    COMPILE-LISP-FORM expr => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun compile-lisp-form (expr)
  (lisp-form expr))

;;
;;  Syntax:
;;
;;    COMPILE-FUNCTION expr compenv scope => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    compenv --- a compiling environment.
;;    scope --- a WAQL symbol.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun compile-function (expr compenv scope)
  (let ((operator (function-operator expr))
        (operands (function-operands expr)))
    (if (lookup-compenv operator compenv)
        (compile-function-letfun operator operands compenv scope)
        (compile-function-built-in operator operands compenv scope))))

(defun compile-function-letfun (operator operands compenv scope)
  (labels ((compile-operands (operands)
             (mapcar #'(lambda (operand)
                         (compile-expression operand compenv scope nil))
                     operands)))
    (cl-pattern:match (lookup-compenv operator compenv)
      ((:letfun largs _ lexpr lcompenv)
       (let ((compiled-operands (compile-operands operands)))
         (let ((compenv1 (add-argvars-compenv largs compiled-operands
                                              lcompenv))
               (scope1 (scoping-symbol operator)))
           (compile-expression lexpr compenv1 scope1 nil))))
      (_ (error "The symbol ~S is bound to variable." operator)))))

(defun compile-function-built-in (operator operands compenv scope)
  (labels ((compile-operands (operands)
             (mapcar #'(lambda (operand)
                         (compile-expression operand compenv scope nil))
                     operands)))
    (let ((compiled-operands (compile-operands operands)))
      `(,operator ,@compiled-operands))))

;;
;;  Syntax:
;;
;;    COMPUTE-LOOKUP-KEYS vars exprs compenv scope lookup-keys => new-lookup-keys
;;
;;  Arguments and Values:
;;
;;    vars --- a list of WAQL symbols.
;;    exprs --- a list of WAQL expressions.
;;    compenv --- a compiling environment.
;;    scope --- a WAQL symbol.
;;    lookup-keys --- a list of lookup-keys.
;;    new-lookup-keys --- a list of lookup-keys.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun compute-lookup-keys (vars exprs compenv scope lookup-keys)
  (append (proper-lookup-keys vars compenv scope)
          (derived-lookup-keys lookup-keys exprs vars)))

(defun proper-lookup-keys (vars compenv scope)
  (let ((n (length vars)))
    (remove nil
      (mapcar #'(lambda (i)
                  (proper-lookup-key i vars compenv scope))
              (iota n)))))

(defun proper-lookup-key (i vars compenv scope)
  (labels ((nullify-underscore-notation (orig-var)
             (and (not (underscore-notation-p orig-var))
                  orig-var)))
    (let ((orig-vars (mapcar #'nullify-underscore-notation
                       (mapcar #'original-symbol vars))))
      (let ((elements (mask-list orig-vars i)))
        (if (some #'identity elements)
            (make-lookup-key elements compenv scope)
            nil)))))

(defun mask-list (list k)
  (loop for x in list
        for i from 0
     if (= i k) collect x
     else collect nil))

(defun derived-lookup-keys (lookup-keys exprs vars)
  (remove nil
    (mapcar #'(lambda (lookup-key)
                (derived-lookup-key lookup-key exprs vars))
            lookup-keys)))

(defun derived-lookup-key (lookup-key exprs vars)
  (match-with-vars vars
    (match-with-exprs exprs lookup-key)))

(defun match-with-vars (vars deriving)
  (destructuring-bind (expr-elem compenv scope) deriving
    (if expr-elem
        (labels ((aux (var)
                   (let ((expr (car expr-elem))
                         (elem (cdr expr-elem)))
                     (if (eq var expr)
                         elem))))
          (let ((elems (mapcar #'aux vars)))
            (if (not (every #'null elems))
                (make-lookup-key elems compenv scope)
                nil))))))

(defun match-with-exprs (exprs lookup-key)
  (labels ((aux (expr elem)
             (if (and elem (variable-reference-p expr))
                 (cons expr elem)
                 nil)))
    (let ((elems (lookup-key-elements lookup-key))
          (compenv (lookup-key-compenv lookup-key))
          (scope (lookup-key-scope lookup-key)))
      (let ((expr-elem (car (remove-if #'null
                              (mapcar #'aux exprs elems)))))
        (list expr-elem compenv scope)))))

;;
;;  Syntax:
;;
;;    COMPILE-LOOKUP-KEYS lookup-keys => new-lookup-keys
;;
;;  Arguments and Values:
;;
;;    lookup-keys --- a list of lookup-keys.
;;    new-lookup-keys --- a list of lookup-keys.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Siglans an error of type type-error if LOOKUP-KEYS is not a list.
;;
(defun compile-lookup-keys (lookup-keys)
  `(list ,@(mapcar #'compile-lookup-key lookup-keys)))

(defun compile-lookup-key (lookup-key)
  (let ((elements (lookup-key-elements lookup-key))
        (compenv (lookup-key-compenv lookup-key))
        (scope (lookup-key-scope lookup-key)))
    (labels ((aux (element)
               (and element
                    (compile-expression element compenv scope nil))))
      `(list ,@(mapcar #'aux elements)))))

;;
;;  Syntax:
;;
;;    MAKE-LOOKUP-KEY elements compenv scope => lookup-key
;;
;;  Arguments and Values:
;;
;;    elements --- a list of WAQL expressions.
;;    compenv --- a compiling environment.
;;    scope --- a WAQL symbol.
;;    lookup-key --- a lookup-key.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if ELEMENTS is not a list.
;;
;;    Signals an error of type type-error if COMPENV is not a compiling
;;    environment.
;;
;;    Signals an error of type type-error if SCOPE is not a WAQL symbol.
;;
(defstruct (lookup-key (:constructor %make-lookup-key))
  (elements :elements :read-only t)
  (compenv :compenv :read-only t)
  (scope :scope :read-only t))

(defun make-lookup-key (elements compenv scope)
  (unless (listp elements)
    (error 'type-error :datum elements :expected-type 'list))
  (unless (compenv-p compenv)
    (error 'type-error :datum compenv :expected-type 'compenv))
  (when scope
    (unless (waql-symbol-p scope)
      (error 'type-error :datum scope :expected-type 'waql-symbol)))
  (%make-lookup-key :elements elements :compenv compenv :scope scope))
