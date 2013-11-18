#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.specialize-function)

;;
;;  Syntax:
;;
;;    SPECIALIZE-FUNCTION-EXPRESSION-TOP expr => new-expr
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
(defun specialize-function-expression-top (expr)
  (let ((typenv (reduce #'(lambda (typenv item)
                            (destructuring-bind (var type _) item
                              (declare (ignore _))
                              (add-letvar-typenv var type typenv)))
                        (predefined-relations)
                        :initial-value (empty-typenv))))
    (specialize-function-expression expr typenv)))

;;
;;  Syntax:
;;
;;    SPECIALIZE-FUNCTION-EXPRESSION expr typenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
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
(defun specialize-function-expression (expr typenv)
  (cond
    ((literal-p expr) (specialize-function-literal expr))
    ((variable-reference-p expr)
     (specialize-function-variable-reference expr))
    ((let-p expr) (specialize-function-let expr typenv))
    ((query-p expr) (specialize-function-query expr typenv))
    ((lisp-form-p expr) (specialize-function-lisp-form expr))
    ((function-p expr) (specialize-function-function expr typenv))
    (t (error "The value ~S is an invalid expression." expr))))

;;
;;  Syntax:
;;
;;    SPECIALIZE-FUNCTION-LITERAL expr => new-expr
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
(defun specialize-function-literal (expr)
  expr)

;;
;;  Syntax:
;;
;;    SPECIALIZE-FUNCTION-VARIABLE-REFERENCE expr typenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
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
(defun specialize-function-variable-reference (expr)
  expr)

;;
;;  Syntax:
;;
;;    SPECIALIZE-FUNCTION-LET expr typenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
;;    new-expr --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun specialize-function-let (expr typenv)
  (cond
    ((let-variable-p expr) (specialize-function-let-var expr typenv))
    ((let-function-p expr) (specialize-function-let-fun expr typenv))
    (t (error "The value ~S is an invalid expression." expr))))

(defun specialize-function-let-var (expr typenv)
  (let ((lvar (let-var expr))
        (llocal (let-local-expr expr))
        (lbody (let-body-expr expr)))
    (let* ((llocal-type (type-of-expression llocal typenv))
           (typenv1 (add-letvar-typenv lvar llocal-type typenv)))
      (let ((llocal1 (specialize-function-expression llocal typenv))
            (lbody1 (specialize-function-expression lbody typenv1)))
        (make-let-variable lvar llocal1 lbody1)))))

(defun specialize-function-let-fun (expr typenv)
  (let ((lvar (let-var expr))
        (largs (let-args expr))
        (larg-types (let-arg-types expr))
        (llocal (let-local-expr expr))
        (lbody (let-body-expr expr)))
    (let ((typenv-local (add-argvars-typenv largs typenv)))
      (let* ((lreturn-type (type-of-expression llocal typenv-local))
             (typenv-body (add-letfun-typenv lvar larg-types lreturn-type
                                             typenv)))
        (let ((llocal1 (specialize-function-expression llocal
                                                       typenv-local))
              (lbody1 (specialize-function-expression lbody typenv-body)))
          (make-let-function lvar largs llocal1 lbody1))))))

;;
;;  Syntax:
;;
;;    SPEICIALIZE-FUNCTION-QUERY expr typenv => new-expr
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
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
(defun specialize-function-query (expr typenv)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (multiple-value-bind (quals1 exprs1)
        (specialize-function-quals quals exprs typenv)
      (make-query exprs1 quals1))))

(defun specialize-function-quals (quals exprs typenv)
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (multiple-value-bind (qual1 rest1 exprs1)
            (specialize-function-qual qual rest exprs typenv)
          (values (cons qual1 rest1) exprs1)))
      (let ((exprs1 (specialize-function-exprs exprs typenv)))
        (values nil exprs1))))

(defun specialize-function-qual (qual rest exprs typenv)
  (if (quantification-p qual)
      (specialize-function-quantification qual rest exprs typenv)
      (specialize-function-predicate qual rest exprs typenv)))

(defun specialize-function-quantification (qual rest exprs typenv)
  (let ((vars (quantification-vars qual))
        (rel (quantification-relation qual)))
    ;; specialize function on the quantification
    (let* ((rel1 (specialize-function-expression rel typenv))
           (qual1 (make-quantification vars rel1)))
      ;; specialize function on REST and EXPRS
      (let* ((rel-type (type-of-expression rel typenv))
             (typenv1 (add-qvars-typenv vars rel-type typenv)))
        (multiple-value-bind (rest1 exprs1)
            (specialize-function-quals rest exprs typenv1)
          (values qual1 rest1 exprs1))))))

(defun specialize-function-predicate (pred rest exprs typenv)
  (let ((pred1 (specialize-function-expression pred typenv)))
    (multiple-value-bind (rest1 exprs1)
        (specialize-function-quals rest exprs typenv)
      (values pred1 rest1 exprs1))))

(defun specialize-function-exprs (exprs typenv)
  (mapcar #'(lambda (expr)
              (specialize-function-expression expr typenv))
          exprs))

;;
;;  Syntax:
;;
;;    SPECIALIZE-FUNCTION-LISP-FORM expr => new-expr
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
(defun specialize-function-lisp-form (expr)
  expr)

;;
;;  Syntax:
;;
;;    SPECIALIZE-FUNCTION-FUNCTION expr typenv => new-typenv
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    typenv --- a type environment.
;;    new-typenv --- a WAQL expression.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun specialize-function-function (expr typenv)
  (let ((operator (function-operator expr)))
    (if (generic-function-p operator)
        (specialize-function-function-generic expr typenv)
        (specialize-function-function-otherwise expr typenv))))

(defun specialize-function-function-generic (expr typenv)
  (labels ((specialize-function-operands (operands)
             (mapcar #'(lambda (operand)
                         (specialize-function-expression operand typenv))
                     operands))
           (type-of-operands (operands)
             (mapcar #'(lambda (operand)
                         (type-of-expression operand typenv))
                     operands)))
    (let ((operator (function-operator expr))
          (operands (function-operands expr)))
      (let ((operands1 (specialize-function-operands operands))
            (operator1 (let* ((operand-types (type-of-operands operands))
                              (func (specialized-function operator
                                                          operand-types)))
                         (specialized-function-name func))))
        `(,operator1 ,@operands1)))))

(defun specialize-function-function-otherwise (expr typenv)
  (let ((operator (function-operator expr))
        (operands (function-operands expr)))
    (let ((operands1 (mapcar #'(lambda (operand)
                                 (specialize-function-expression operand
                                                                 typenv))
                             operands))
          (operator1 operator))
      `(,operator1 ,@operands1))))
