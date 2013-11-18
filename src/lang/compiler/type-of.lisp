#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.type-of)


;;
;;  Syntax:
;;
;;    TYPE-OF-EXPRESSION-TOP expr => type
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
;;    None.
;;
(defun type-of-expression-top (expr)
  (let ((typenv (reduce #'(lambda (typenv item)
                            (destructuring-bind (var type _) item
                              (declare (ignore _))
                              (add-letvar-typenv var type typenv)))
                        (predefined-relations)
                        :initial-value (empty-typenv))))
    (type-of-expression expr typenv)))


;;
;;  Syntax:
;;
;;    TYPE-OF-EXPRESSION expr typenv => type
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
;;    type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun type-of-expression (expr typenv)
  (cond
    ((literal-p expr) (type-of-literal expr))
    ((variable-reference-p expr)
     (type-of-variable-reference expr typenv))
    ((let-p expr) (type-of-let expr typenv))
    ((query-p expr) (type-of-query expr typenv))
    ((lisp-form-p expr) (type-of-lisp-form expr))
    ((function-p expr) (type-of-function expr typenv))
    (t (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    TYPE-OF-LITERAL expr => type
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
;;    None.
;;
(defun type-of-literal (expr)
  (cond
    ((int-literal-p expr) :int)
    ((string-literal-p expr) :string)
    ((time-literal-p expr) :time)
    (t (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    TYPE-OF-VARIABLE-REFERENCE expr typenv => type
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
;;    type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if TYPENV is not a type
;;    environment.
;;
;;    Signals an error of type simple-error if EXPR is unbound in 
;;    TYPENV.
;;    
;;    Signals an error of type simple-error if EXPR is bound to a function
;;    in TYPENV.
;;
(defun type-of-variable-reference (expr typenv)
  (let ((type (lookup-typenv expr typenv)))
    (unless type
      (error "The variable ~S is an unbound variable." expr))
    (unless (not (function-type-p type))
      (error "The variable ~S is bound to a function." expr))
    type))

;;
;;  Syntax:
;;
;;    TYPE-OF-LET expr typenv => type
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
;;    type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun type-of-let (expr typenv)
  (cond
    ((let-variable-p expr) (type-of-let-var expr typenv))
    ((let-function-p expr) (type-of-let-fun expr typenv))
    (t (error "The value ~S is an invalid expression." expr))))

(defun type-of-let-var (expr typenv)
  (let ((lvar (let-var expr))
        (llocal (let-local-expr expr))
        (lbody (let-body-expr expr)))
    (let* ((llocal-type (type-of-expression llocal typenv))
           (typenv1 (add-letvar-typenv lvar llocal-type typenv)))
      (type-of-expression lbody typenv1))))

(defun type-of-let-fun (expr typenv)
  (let ((lvar (let-var expr))
        (largs (let-args expr))
        (larg-types (let-arg-types expr))
        (llocal (let-local-expr expr))
        (lbody (let-body-expr expr)))
    (let* ((ltypenv (add-argvars-typenv largs typenv))
           (lreturn-type (type-of-expression llocal ltypenv))
           (typenv1 (add-letfun-typenv lvar larg-types lreturn-type
                                       typenv)))
      (type-of-expression lbody typenv1))))

;;
;;  Syntax:
;;
;;    TYPE-OF-QUERY expr typenv => type
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
;;    type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if a predicate of EXPR does not
;;    return a value of :bool type.
;;
(defun type-of-query (expr typenv)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (type-of-quals quals exprs typenv)))

(defun type-of-quals (quals exprs typenv)
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (type-of-qual qual rest exprs typenv))
      (type-of-exprs exprs typenv)))

(defun type-of-qual (qual rest exprs typenv)
  (if (quantification-p qual)
      (type-of-quantification qual rest exprs typenv)
      (type-of-predicate qual rest exprs typenv)))

(defun type-of-quantification (qual rest exprs typenv)
  (let ((vars (quantification-vars qual))
        (relation (quantification-relation qual)))
    (let* ((relation-type (type-of-expression relation typenv))
           (typenv1 (add-qvars-typenv vars relation-type typenv)))
      (type-of-quals rest exprs typenv1))))

(defun type-of-predicate (pred rest exprs typenv)
  (unless (eq (type-of-expression pred typenv) :bool)
    (error "The value ~S should return a value of :bool type." pred))
  (type-of-quals rest exprs typenv))

(defun type-of-exprs (exprs typenv)
  (let ((attr-types (mapcar #'(lambda (expr)
                                (type-of-expression expr typenv))
                            exprs)))
    (make-relation-type attr-types)))

;;
;;  Syntax:
;;
;;    TYPE-OF-LISP-FORM expr => type
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
;;    None.
;;
(defun type-of-lisp-form (expr)
  (lisp-form-type expr))

;;
;;  Syntax:
;;
;;    TYPE-OF-FUNCTION expr typenv => type
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
;;    type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if TYPENV is not a type
;;    environment.
;;
;;    Signals an error of type simple-error if operator of EXPR is
;;    undefined in TYPENV.
;;
;;    Signals an error of type simple-error if operator of EXPR is
;;    bound to a variable in TYPENV.
;;
;;    Signals an error of type simple-error if number of operands of EXPR
;;    is invalid.
;;
;;    Signals an error of type simple-error if types of operands of EXPR
;;    are invalid.
;;
(defun type-of-function (expr typenv)
  (let ((operator (function-operator expr)))
    (if (waql-symbol-p operator)
        (cond
          ((lookup-typenv operator typenv)
           (type-of-function-letfun expr typenv))
          ((generic-function-p operator)
           (type-of-function-built-in expr typenv))
          (t (error "The function ~S is undefined." operator)))
        (type-of-function-built-in expr typenv))))

(defun type-of-function-letfun (expr typenv)
  (let* ((operator (function-operator expr))
         (operands (function-operands expr)))
    (let ((operand-types (mapcar #'(lambda (operand)
                                     (type-of-expression operand typenv))
                                 operands)))
      (let* ((fun-type (lookup-typenv operator typenv))
             (arg-types (function-type-argument-types fun-type))
             (return-type (function-type-return-type fun-type)))
        (unless (function-type-p fun-type)
          (error "The symbol ~S is bound to a variable." operator))
        (unless (length= arg-types operand-types)
          (error "The number of arguments of ~S is invalid." expr))
        (unless (equal arg-types operand-types)
          (error "The types of arguments of ~S are invalid." expr))
        return-type))))

(defun type-of-function-built-in (expr typenv)
  (let ((operator (function-operator expr))
        (operands (function-operands expr)))
    (let* ((operand-types (mapcar #'(lambda (operand)
                                      (type-of-expression operand typenv))
                                  operands))
           (func (specialized-function operator operand-types)))
      (specialized-function-return-type func))))
