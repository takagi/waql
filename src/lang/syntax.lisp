#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.syntax)

;;
;;  Syntax:
;;
;;    LITERAL-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    Returns true if EXPR is a WAQL expression of literal, or returns
;;    false.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun literal-p (expr)
  (or (int-literal-p expr)
      (string-literal-p expr)
      (time-literal-p expr)))

;;
;;  Syntax:
;;
;;    INT-LITERAL-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun int-literal-p (expr)
  (waql-integer-p expr))

;;
;;  Syntax:
;;
;;    STRING-LITERAL-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun string-literal-p (expr)
  (waql-string-p expr))

;;
;;  Syntax:
;;
;;    MAKE-TIME-LITERAL date-string time-string => time-literal
;;
;;  Arguments and Values:
;;
;;    date-string --- a string.
;;    time-string --- a string.
;;    time-literal --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if DATE-STRING is not a string.
;;
;;    Siglans an error of type type-error if TIME-STRING is not a string.
;;
(defun make-time-literal (date-string time-string)
  (unless (waql-string-p date-string)
    (error 'type-error :datum date-string :expected-type 'waql-string))
  (unless (waql-string-p time-string)
    (error 'type-error :datum time-string :expected-type 'waql-string))
  (list 'time date-string time-string))

;;
;;  Syntax:
;;
;;    TIME-LITERAL-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun time-literal-p (expr)
  (cl-pattern:match expr
    (('time . _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    TIME-LITERAL-DATE expr => date-string
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    date-string --- a string.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of time literal.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of time literal.
;;
(defun time-literal-date (expr)
  (cl-pattern:match expr
    (('time date-string _) date-string)
    (('time . _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    TIME-LITERAL-TIME expr => time-string
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    time-string --- a string.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of time literal.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of time literal.
;;
(defun time-literal-time (expr)
  (cl-pattern:match expr
    (('time _ time-string) time-string)
    (('time . _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    VARIABLE-REFERENCE-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun variable-reference-p (expr)
  (waql-symbol-p expr))

;;
;;  Syntax:
;;
;;    MAKE-LET-VARIABLE var local-expr body-expr => let-variable
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    local-expr --- a WAQL expression.
;;    body-expr --- a WAQL expression.
;;    let-variable --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
(defun make-let-variable (var local-expr body-expr)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  `(let (,var ,local-expr) ,body-expr))

;;
;;  Syntax:
;;
;;    MAKE-LET-FUNCTION var arg-list local-expr body-expr => let-function
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    arg-list --- a list of arguments.
;;    local-expr --- a WAQL expression.
;;    body-expr --- a WAQL expression.
;;    let-function --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type simple-error if ARG-LIST is not a list of
;;    arguments.
;;
(defun make-let-function (var arg-list local-expr body-expr)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (dolist (arg arg-list)
    (unless (argument-p arg)
      (error "The value ~S is not an argument." arg)))
  `(let (,var ,arg-list ,local-expr) ,body-expr))

;;
;;  Syntax:
;;
;;    LET-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun let-p (expr)
  (cl-pattern:match expr
    (('let . _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    LET-VARIABLE-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun let-variable-p (expr)
  (cl-pattern:match expr
    (('let (_ _) _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    LET-FUNCTION-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun let-function-p (expr)
  (cl-pattern:match expr
    (('let (_ _ _) _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    LET-VAR expr => var
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    var --- a WAQL symbol.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of let binding.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of let binding.
;;
(defun let-var (expr)
  ;; use OPTIMA instead of CL-PATTERN because of uncapability
  ;; in this case
  (optima:match expr
    ((list 'let (list var _) _) var)
    ((list 'let (list var _ _) _) var)
    ((cons 'let _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    LET-ARGS expr => arg-list
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    arg-list --- a list of arguments.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a WAQL expression of
;;    let binding for a variable.
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of let binding.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of let binding.
;;
(defun let-args (expr)
  ;; use OPTIMA instead of CL-PATTERN because of uncapability
  ;; in this case
  (optima:match expr
    ((list 'let (list _ args _) _) args)
    ((list 'let (list _ _) _)
     (error "The value ~S is a variable binding." expr))
    ((cons 'let _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    LET-ARG-VARS expr => var-list
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    var-list --- a list of WAQL symbols.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to LET-ARGS function.
;;
(defun let-arg-vars (expr)
  (mapcar #'argument-var (let-args expr)))

;;
;;  Syntax:
;;
;;    LET-ARG-TYPES expr => type-list
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    type-list --- a list of WAQL types.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to LET-ARGS function.
;;
(defun let-arg-types (expr)
  (mapcar #'argument-type (let-args expr)))

;;
;;  Syntax:
;;
;;    LET-LOCAL-EXPR expr => local-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    local-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of let binding.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of let binding.
;;
(defun let-local-expr (expr)
  ;; use OPTIMA instead of CL-PATTERN because of uncapability
  ;; in this case
  (optima:match expr
    ((list 'let (list _ local-expr) _) local-expr)
    ((list 'let (list _ _ local-expr) _) local-expr)
    ((cons 'let _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    LET-BODY-EXPR expr => body-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    body-expr a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of let binding.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of let binding.
;;
(defun let-body-expr (expr)
  ;; use OPTIMA instead of CL-PATTERN because of uncapability
  ;; in this case
  (optima:match expr
    ((list 'let (list _ _) body-expr) body-expr)
    ((list 'let (list _ _ _) body-expr) body-expr)
    ((cons 'let _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invaild expression." expr))))

;;
;;  Syntax:
;;
;;    MAKE-ARGUMENT var type => argument
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    type --- a WAQL type.
;;    argument --- an argument.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if TYPE is not a WAQL type.
;;
(defun make-argument (var type)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (unless (waql-type-p type)
    (error 'type-error :datum type :expected-type 'waql-type))
  (list var type))

;;
;;  Syntax:
;;
;;    ARGUMENT-P arg => boolean
;;
;;  Arguments and Values:
;;
;;    arg --- an argument.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun argument-p (arg)
  (cl-pattern:match arg
    ((_ _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    ARGUMENT-VAR arg => var
;;
;;  Arguments and Values:
;;
;;    arg --- an argument.
;;    var --- a WAQL symbol.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if ARG is not an argument.
;;
(defun argument-var (arg)
  (cl-pattern:match arg
    ((var _) var)
    (_ (error "The value ~S is an invalid argument." arg))))

;;
;;  Syntax:
;;
;;    ARGUMENT-TYPE arg => type
;;
;;  Arguments and Values:
;;
;;    arg --- an argument.
;;    type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if ARG is not an argument.
;;
(defun argument-type (arg)
  (cl-pattern:match arg
    ((_ type) type)
    (_ (error "The value ~S is an invalid argument." arg))))

;;
;;  Syntax:
;;
;;    MAKE-QUERY expr-list qual-list => query
;;
;;  Arguments and Values:
;;
;;    expr-list --- a list of WAQL expressions.
;;    qual-list --- a list of qualifications.
;;    query --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if EXPR-LIST is not a list of
;;    WAQL expressions.
;;
;;    Signals an error of type type-error if QUAL-LIST is not a list of
;;    qualifications.
;;
(defun make-query (expr-list qual-list)
  (unless (listp expr-list)
    (error 'type-error :datum expr-list :expected-type 'list))
  (unless (listp qual-list)
    (error 'type-error :datum qual-list :expected-type 'list))
  `(query ,expr-list ,@qual-list))

;;
;;  Syntax:
;;
;;    QUERY-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun query-p (expr)
  (cl-pattern:match expr
    (('query . _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    QUERY-EXPRS expr => expr-list
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    expr-list --- a list of WAQL expressions.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of query.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of query.
;;
(defun query-exprs (expr)
  (cl-pattern:match expr
    (('query expr-list . _) expr-list)
    (('query . _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    QUERY-QUALS expr => qual-list
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    qual-list --- a list of qualifications.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of query.
;;
;;    Signals an error of type simple-error if EXPR is a WAQL expression of
;;    query.
;;
(defun query-quals (expr)
  (cl-pattern:match expr
    (('query _ . qual-list) qual-list)
    (('query . _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    MAKE-QUANTIFICATION var-list relation-expr => quantification
;;
;;  Arguments and Values:
;;
;;    var-list --- a list of WAQL symbols.
;;    relation-expr --- a WAQL expression.
;;    quantification --- a quantification.
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
(defun make-quantification (var-list relation-expr)
  (dolist (var var-list)
    (unless (or (waql-symbol-p var)
                (underscore-notation-p var))
      (error 'type-error :datum var :expected-type 'waql-symbol)))
  `(<- ,var-list ,relation-expr))

;;
;;  Syntax:
;;
;;    QUANTIFICATION-P quantification => boolean
;;
;;  Arguments and Values:
;;
;;    quantification --- a quantification.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun quantification-p (quantification)
  (cl-pattern:match quantification
    (('<- . _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    QUANTIFICATION-VARS quantification => var-list
;;
;;  Arguments and Values:
;;
;;    quantification --- a quantification.
;;    var-list --- a list of WAQL symbols.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if quantification is a
;;    malformed quantification.
;;
;;    Signals an error of type simple-error if QUANTIFICATION is not a
;;    quantification.
;;
(defun quantification-vars (quantification)
  (cl-pattern:match quantification
    (('<- vars _) vars)
    (('<- . _) (error "The value ~S is malformed." quantification))
    (_ (error "The value ~S is an invalid expression." quantification))))

;;
;;  Syntax:
;;
;;    QUANTIFICATION-RELATION quantification => relation-expr
;;
;;  Arguments and Values:
;;
;;    quantification --- a quantification.
;;    relation-expr --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if quantification is 
;;    malformed quantification.
;;
;;    Signals an error of type simple-error if QUANTIFICATION is not a
;;    quantification.
;;
(defun quantification-relation (quantification)
  (cl-pattern:match quantification
    (('<- _ relation) relation)
    (('<- . _) (error "The value ~S is malformed." quantification))
    (_ (error "The value ~S is an invalid expression." quantification))))

;;
;;  Syntax:
;;
;;    MAKE-LISP-FORM form type => lisp-form
;;
;;  Arguments and Values:
;;
;;    form --- a form.
;;    type --- a WAQL type.
;;    lisp-form --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if TYPE is not a WAQL type.
;;
(defun make-lisp-form (form type)
  (unless (waql-type-p type)
    (error 'type-error :datum type :expected-type 'waql-type))
  `(lisp ,form ,type))

;;
;;  Syntax:
;; 
;;    LISP-FORM-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun lisp-form-p (expr)
  (cl-pattern:match expr
    (('lisp . _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    LISP-FORM expr => form
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
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of lisp form.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of lisp form.
;;
(defun lisp-form (expr)
  (cl-pattern:match expr
    (('lisp form _) form)
    (('lisp) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    LISP-FORM-TYPE expr => type
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is a malformed WAQL
;;    expression of lisp form.
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of lisp form.
;;
(defun lisp-form-type (expr)
  (cl-pattern:match expr
    (('lisp _ type) type)
    (('lisp . _) (error "The value ~S is malformed." expr))
    (_ (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    MAKE-FUNCTION operator operands => function-application
;;
;;  Arguments and Values:
;;
;;    operator --- a WAQL symbol.
;;    operands --- a list of WAQL expressions.
;;    function-application --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun make-function (operator operands)
  `(,operator ,@operands))

;;
;;  Syntax:
;;
;;    FUNCTION-P expr => boolean
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    Returns true if EXPR is a WAQL expression of function application,
;;    or returns false.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun function-p (expr)
  (cl-pattern:match expr
    ((_ . _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    FUNCTION-OPERATOR expr => function-name
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    function-name --- a WAQL symbol.
;;
;;  Description:
;;
;;    Returns the operator part of EXPR which is a WAQL expression of
;;    function application.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of function application.
;;
(defun function-operator (expr)
  (cl-pattern:match expr
    ((operator . _) operator)
    (_ (error "The value ~S is an invalid expression."))))

;;
;;  Syntax:
;;
;;    FUNCTION-OPERANDS expr => operand-list
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    operand-list --- a list of WAQL expressions.
;;
;;  Description:
;;
;;    Returns the operands part of EXPR which is a WAQL expression of
;;    function application.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if EXPR is not a WAQL
;;    expression of function application.
;;
(defun function-operands (expr)
  (cl-pattern:match expr
    ((_ . operands) operands)
    (_ (error "The value ~S is an invalid expression."))))


