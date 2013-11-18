#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.compenv)

;;
;;  Syntax:
;;
;;    EMPTY-COMPENV => empty-compenv
;;
;;  Arguments and Values:
;;
;;    empty-compenv --- a compiling environment.
;;
;;  Description:
;;
;;    Returns an empty compiling environment.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defstruct (compenv (:constructor %make-compenv)
                    (:conc-name %compenv-))
  (elements :elements :read-only t))

(defmacro with-compenv-elements ((elements compenv) &body body)
  `(let ((,elements (%compenv-elements ,compenv)))
     (%make-compenv :elements (progn ,@body))))

(defun empty-compenv ()
  (%make-compenv :elements nil))

;;
;;  Syntax:
;;
;;    ADD-QVAR-COMPENV var type compenv => new-compenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    type --- a WAQL type.
;;    compenv --- a compiling environment.
;;    new-compenv --- a compiling environment.
;;
;;  Description:
;;
;;    Constructs a new compiling environment by adding (:qvar TYPE) to
;;    COMPENV. If VAR already exists, the older ones are shadowed.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if TYPE is not a WAQL type.
;;
;;    Signals an error of type type-error if COMPENV is not a compiling
;;    environment.
;;
(defun add-qvar-compenv (var type compenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (unless (waql-type-p type)
    (error 'type-error :datum type :expected-type 'waql-type))
  (with-compenv-elements (elements compenv)
    (acons var (list :qvar type) elements)))

;;
;;  Syntax:
;;
;;    ADD-ARGVAR-COMPENV var type expr compenv => new-compenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    type --- a WAQL type.
;;    expr --- a WAQL expression.
;;    compenv --- a compiling environment.
;;    new-compenv --- a compiling environment.
;;
;;  Description:
;;
;;    Constructs a new compiling environment by adding (:argvar TYPE EXPR)
;;    to COMPENV. If VAR already exists, the older ones are shadowed.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if TYPE is not a WAQL type.
;;
;;    Signals an error of type type-error if COMPENV is not a compiling
;;    environment.
;;
(defun add-argvar-compenv (var type expr compenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (unless (waql-type-p type)
    (error 'type-error :datum type :expected-type 'waql-type))
  (with-compenv-elements (elements compenv)
    (acons var (list :argvar type expr) elements)))

;;
;;  Syntax:
;;
;;    ADD-LETVAR-COMPENV var type expr compenv => new-compenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    type --- a WAQL type.
;;    expr --- an WAQL expression.
;;    compenv --- a compiling environment.
;;    new-compenv --- a compiling environment.
;;
;;  Description:
;;
;;    Constructs a new compiling environment by adding
;;    (:letvar TYPE EXPR COMPENV) to COMPENV. If VAR already exists, the
;;    older ones are shadowed.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if TYPE is not a WAQL type.
;;
;;    Siglans an error of type type-error if COMPENV is not a compiling
;;    environment.
;;
(defun add-letvar-compenv (var type expr compenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (unless (waql-type-p type)
    (error 'type-error :datum type :expected-type 'waql-type))
  (with-compenv-elements (elements compenv)
    (acons var (list :letvar type expr compenv) elements)))

;;
;;  Syntax:
;;
;;    ADD-LETFUN-COMPENV var arg-list return-type expr compenv => new-compenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    arg-list --- a list of arguments.
;;    return-type --- a WAQL type.
;;    expr --- a WAQL expression.
;;    compenv --- a compiling environment.
;;    new-compenv --- a compiling environment.
;;
;;  Description:
;;
;;    Constructs a new compiling environment by adding
;;    (:letfun ARG-LIST RETURN-TYPE EXPR COMPENV) to COMPENV. If VAR
;;    already exists, the older ones are shadowed.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type simple-error if ARG-LIST is not a list of
;;    arguments.
;;
;;    Signals an error of type type-error if RETURN-TYPE is not a WAQL
;;    type.
;;
;;    Signals an error of type type-error if COMPENV is not a compiling
;;    environment.
;;
(defun add-letfun-compenv (var arg-list return-type expr compenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (dolist (arg arg-list)
    (unless (argument-p arg)
      (error "The value ~S is not an argument." arg)))
  (unless (waql-type-p return-type)
    (error 'type-error :datum return-type :expected-type 'waql-type))
  (with-compenv-elements (elements compenv)
    (acons var (list :letfun arg-list return-type expr compenv)
           elements)))

;;
;;  Syntax:
;;
;;    ADD-QVARS-COMPENV var-list type-list compenv => new-compenv
;;
;;  Arguments and Values:
;;
;;    var-list --- a list of WAQL symbols.
;;    type-list --- a list of WAQL types.
;;    compenv --- a compiling environment.
;;    new-compenv --- a compiling environment.
;;
;;  Description:
;;
;;    Equivalent to ADD-QVAR-COMPENV function except that receiving
;;    multiple symbols with their types as VAR-LIST and TYPE-LIST.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if the lengths of VAR-LIST
;;    and TYPE-LIST are not equal.
;;
;;    The other situations are equivalent to ADD-QVAR-COMPENV function.
;;
(defun add-qvars-compenv (var-list type-list compenv)
  (unless (length= var-list type-list)
    (error "The lengths of ~S and ~S are different." var-list type-list))
  (let ((var-type-list (mapcar #'cons var-list type-list)))
    (reduce #'(lambda (compenv var-type)
                (destructuring-bind (var . type) var-type
                  (add-qvar-compenv var type compenv)))
            var-type-list :initial-value compenv)))

;;
;;  Syntax:
;;
;;    ADD-ARGVARS-COMPENV arg-list expr-list compenv => new-compenv
;;
;;  Arguments and values:
;;
;;    arg-list --- a list of arguments.
;;    expr-list --- a list of WAQL expressions.
;;    compenv --- a compiling environment.
;;    new-compenv --- a compiling environment.
;;
;;  Description:
;;
;;    Equivalent to ADD-ARGVAR-COMPENV function except receiving VAR,
;;    TYPE and EXPR as lists.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to ADD-ARGVAR-COMPENV function, ARGUMENT-VAR function
;;    and ARGUMENT-TYPE function.
;;
(defun add-argvars-compenv (arg-list expr-list compenv)
  (let ((arg-expr-list (mapcar #'cons arg-list expr-list)))
    (reduce #'(lambda (compenv arg-expr)
                (destructuring-bind (arg . expr) arg-expr
                  (let ((arg-var (argument-var arg))
                        (arg-type (argument-type arg)))
                    (add-argvar-compenv arg-var arg-type expr compenv))))
            arg-expr-list :initial-value compenv)))

;;
;;  Syntax:
;;
;;    LOOKUP-COMPENV var compenv => element
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    compenv --- a compiling environment.
;;    element --- a list which is an element of COMPENV, or nil.
;;
;;  Description:
;;
;;    Returns a list corresponding to VAR in COMPENV, or NIL if VAR does
;;    not exist.
;;
;;    The table of adding interfaces and returned elements is as below:
;;
;;      ADD-QVAR-COMPENV   (:qvar TYPE)
;;      ADD-ARGVAR-COMPENV (:argvar TYPE EXPR)
;;      ADD-LETVAR-COMPENV (:letvar TYPE EXPR)
;;      ADD-LETFUN-COMPENV (:letfun ARG-LIST RETURN-TYPE EXPR)
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if COMPENV is not a compiling
;;    environment.
;;
(defun lookup-compenv (var compenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (let ((elements (%compenv-elements compenv)))
    (cdr (assoc var elements))))

;;
;;  Syntax:
;;
;;    COMPENV->TYPENV compenv => typenv
;;
;;  Arguments and Values:
;;
;;    compenv --- a compiling environment.
;;    typenv --- a type environment.
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
(defun compenv->typenv (compenv)
  (let ((elements (%compenv-elements compenv))
        (typenv (empty-typenv)))
    (%compenv->typenv elements typenv)))

(defun %compenv->typenv (elements typenv)
  (if elements
      (let ((element (car elements))
            (rest (cdr elements)))
        (let ((typenv1 (%compenv->typenv rest typenv)))
          (cl-pattern:match element
            ((var :qvar type) (add-qvar-typenv var type typenv1))
            ((var :argvar type _) (add-argvar-typenv var type typenv1))
            ((var :letvar type _ _) (add-letvar-typenv var type typenv1))
            ((var :letfun arg-list return-type _ _)
             (let ((arg-types (mapcar #'argument-type arg-list)))
               (add-letfun-typenv var arg-types return-type typenv1)))
            (_ (error "The element ~S is invalid." element)))))
      typenv))
