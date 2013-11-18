#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.typenv)

;;
;;  Syntax:
;;
;;    EMPTY-TYPENV => empty-typenv
;;
;;  Arguments and Values:
;;
;;    empty-typenv --- a type environment.
;;
;;  Description:
;;
;;    Returns an empty type environment.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defstruct (typenv (:constructor %make-typenv)
                   (:conc-name %typenv-))
  (elements :elements :read-only t))

(defmacro with-typenv-elements ((elements typenv) &body body)
  `(let ((,elements (%typenv-elements ,typenv)))
     (%make-typenv :elements (progn ,@body))))

(defun empty-typenv ()
  (%make-typenv :elements nil))

;;
;;  Syntax:
;;
;;    ADD-TYPENV var type typenv => new-typenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    type --- a WAQL type.
;;    typenv --- a type environment.
;;    new-typenv --- a type environment.
;;
;;  Description:
;;
;;    Constructs a new type environment by adding a cons containing
;;    VAR and TYPE to TYPENV. If VAR already exists, the older
;;    ones are shadowed.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if TYPE is not a WAQL type.
;;
;;    Signals an error of type type-error if TYPENV is not a type
;;    environment.
;;
(defun add-typenv (var type typenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (unless (waql-type-p type)
    (error 'type-error :datum type :expected-type 'waql-type))
  (with-typenv-elements (elements typenv)
    (acons var type elements)))

;;
;;  Syntax:
;;
;;    ADD-QVAR-TYPENV var type typenv => new-typenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    type --- a WAQL type.
;;    typenv --- a type environment.
;;    new-typenv --- a type environment.
;;
;;  Description:
;;
;;    Equivalent to ADD-TYPENV function.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if VAR already exists in
;;    TYPENV.
;;
;;    Equivalent to ADD-TYPENV function.
;;
(defun add-qvar-typenv (var type typenv)
  (unless (null (lookup-typenv var typenv))
    (error "The variables ~S already exists in the type environment."
           var))
  (add-typenv var type typenv))

;;
;;  Syntax:
;;
;;    ADD-ARGVAR-TYPENV var type typenv => new-typenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    type --- a WAQL type.
;;    typenv --- a type environment.
;;    new-typenv --- a type environment.
;;
;;  Description:
;;
;;    Equivalent to ADD-TYPENV function.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to ADD-TYPENV function.
;;
(defun add-argvar-typenv (var type typenv)
  (add-typenv var type typenv))

;;
;;  Syntax:
;;
;;    ADD-LETVAR-TYPENV var type typenv => new-typenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    type --- a WAQL type.
;;    typenv --- a type environemnt.
;;    new-typenv --- a type environment.
;;
;;  Description:
;;
;;    Equivalent to ADD-TYPENV function.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to ADD-TYPENV function.
;;
(defun add-letvar-typenv (var type typenv)
  (add-typenv var type typenv))

;;
;;  Syntax:
;;
;;    ADD-LETFUN-TYPENV var arg-types return-type typenv => new-typenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    arg-types --- a list of WAQL types.
;;    return-type --- a WAQL type.
;;    typenv --- a type environment.
;;    new-typenv --- a type environment.
;;
;;  Description:
;;
;;    Equivalent to ADD-TYPENV function except that a function type
;;    constructed from ARG-TYPES and RETURN-TYPE is used as VAR's type.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to ADD-TYPENV function and MAKE-FUNCTION-TYPE function.
;;
(defun add-letfun-typenv (var arg-types return-type typenv)
  (let ((fun-type (make-function-type arg-types return-type)))
    (add-typenv var fun-type typenv)))

;;
;;  Syntax:
;;
;;    ADD-QVARS-TYPENV var-list relation-type typenv => new-typenv
;;
;;  Arguments and Values:
;;
;;    var-list --- a list of WAQL symbols.
;;    relation-type --- a WAQL type.
;;    typenv --- a type environment.
;;    new-type --- a type environment.
;;
;;  Description:
;;
;;    Equivalent to ADD-QVAR-TYPENV function except that receiving
;;    a list of variables and a relation type as VAR-LIST and
;;    RELATION-TYPE.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if RELATION-TYPE is not a
;;    relation type.
;;
;;    Signals an error of type simple-error if the length of VAR-LIST
;;    does not match the dimension of RELATION-TYPE.
;;
;;    The other situations are equivalent to ADD-QVAR-TYPENV function.
;;
(defun add-qvars-typenv (var-list relation-type typenv)
  (unless (= (length var-list) (relation-type-dimension relation-type))
    (error "The length of ~S does not match the dimension of ~S."
           var-list relation-type))
  (let* ((type-list (relation-type-attributes relation-type))
         (var-type-list (mapcar #'cons var-list type-list)))
    (reduce #'(lambda (typenv var-type)
                (destructuring-bind (var . type) var-type
                  (add-qvar-typenv var type typenv)))
            var-type-list :initial-value typenv)))

;;
;;  Syntax:
;;
;;    ADD-ARGVARS-TYPENV arg-list typenv => new-typenv
;;
;;  Arguments and Values:
;;
;;    arg-list --- a list of arguments.
;;    typenv --- a type environment.
;;    new-typenv --- a type environment.
;;
;;  Description:
;;
;;    Equivalent to ADD-ARGVAR-TYPENV function except that receiving
;;    multiple variables with their types as ARG-LIST.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to ADD-ARGVAR-TYPENV function, ARGUMENT-VAR function and
;;    ARGUMENT-TYPE function.
;;
(defun add-argvars-typenv (arg-list typenv)
  (reduce #'(lambda (typenv arg)
              (let ((arg-var (argument-var arg))
                    (arg-type (argument-type arg)))
                (add-argvar-typenv arg-var arg-type typenv)))
          arg-list :initial-value typenv))

;;
;;  Syntax:
;;
;;    LOOKUP-TYPENV var typenv => type
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    typenv --- a type environment.
;;    type --- a WAQL type.
;;
;;  Description:
;;
;;    Returns a type corresponding to VAR in TYPENV, or NIL if VAR
;;    does not exist.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if TYPENV is not a type
;;    environment.
;;
(defun lookup-typenv (var typenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (let ((elements (%typenv-elements typenv)))
    (cdr (assoc var elements))))
