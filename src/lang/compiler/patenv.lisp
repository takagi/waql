#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.patenv)

;;
;;  Syntax:
;;
;;    EMPTY-PATENV => empty-patenv
;;
;;  Arguments and Values:
;;
;;    empty-patenv --- a pattern matching environment.
;;
;;  Description:
;;
;;    Returns an empty pattern matching environment.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defstruct (patenv (:constructor %make-patenv)
                   (:conc-name %patenv-))
  (elements :elements :read-only t))

(defmacro with-patenv-elements ((elements patenv) &body body)
  `(let ((,elements (%patenv-elements ,patenv)))
     (%make-patenv :elements (progn ,@body))))

(defun empty-patenv ()
  (%make-patenv :elements nil))

;;
;;  Syntax:
;;
;;    ADD-PATENV var patenv => new-patenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    patenv --- a pattern matching environment.
;;    new-patenv --- a pattern matching environment.
;;
;;  Description:
;;
;;    Constructs a new pattern matching environment by adding VAR and
;;    corresponding counter initialized by one to PATENV.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if PATENV is not a pattern
;;    matching environment.
;;
;;    Signals an error of type simple-error if VAR already exists in
;;    PATENV.
;;
(defun add-patenv (var patenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (unless (null (lookup-patenv var patenv))
    (error "The variable ~S already exists." var))
  (with-patenv-elements (elements patenv)
    (acons var 1 elements)))

;;
;;  Syntax:
;;
;;    BULK-ADD-PATENV var-list patenv => new-patenv
;;
;;  Arguments and Values:
;;
;;    var-list --- a list of WAQL symbols.
;;    patenv --- a pattern matching environment.
;;    new-patenv --- a pattern matching environment.
;;
;;  Description:
;;
;;    Constructs a new pattern matching environment by adding VAR-LIST
;;    recursively to PATENV through ADD-PATENV function.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR-LIST is not a list.
;;
(defun bulk-add-patenv (var-list patenv)
  (reduce #'(lambda (patenv var)
              (add-patenv var patenv))
          var-list :initial-value patenv))

;;
;;  Syntax:
;;
;;    INC-PATENV var patenv => new-patenv
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    patenv --- a pattern matching environment.
;;    new-patenv --- a pattern matching environment.
;;
;;  Description:
;;
;;    Constructs a new pattern matching environment by incrementing
;;    the counter corresponding to VAR in PATENV.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if PATENV is not a pattern
;;    matching environment.
;;
;;    Signals an error of type simple-error if VAR does not exist in
;;    PATENV.
;;
(defun inc-patenv (var patenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (labels ((%inc-patenv (var elements)
             (cl-pattern:match elements
               (((var1 . cnt) . rest)
                (if (eq var1 var)
                    (acons var1 (1+ cnt) rest)
                    (acons var1 cnt (%inc-patenv var rest))))
               (_ (error "The variable ~S does not exist." var)))))
    (with-patenv-elements (elements patenv)
      (%inc-patenv var elements))))

;;
;;  Syntax:
;;
;;    LOOKUP-PATENV var patenv => element
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    patenv --- a pattern matching environment.
;;    element --- a cons which is an element of PATENV, or nil.
;;
;;  Description:
;;
;;    Returns a cons containing VAR and corresponding counter in PATENV,
;;    or NIL if VAR does not exist.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if PATENV is not a pattern
;;    matching environment.
;;
(defun lookup-patenv (var patenv)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (let ((elements (%patenv-elements patenv)))
    (assoc var elements)))
