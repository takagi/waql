#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.defrelation)

;;
;;  Syntax:
;;
;;    EMPTY-PREDEFINED-RELATIONS => empty-predefined-relations
;;
;;  Arguments and Values:
;;
;;    empty-predefined-relations --- a list of predefined relations.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun empty-predefined-relations ()
  nil)

(defvar *predefined-relations* (empty-predefined-relations))

;;
;;  Syntax:
;;
;;    ADD-PREDEFINED-RELATION var attribute-types predefined-relations => new-predefined-relations
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    attribute-types --- a list of WAQL types.
;;    predefined-relations --- a list of predefined relations.
;;
;;  Description:
;;
;;    Constructs a new list of predefined relations by adding
;;    (VAR . ATTRIBUTES-TYPES) to PREDEFINED-RELATIONS. If VAR already
;;    exists, the older one will be overwritten.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if ATTRIBUTE-TYPES is not a list
;;    of WAQL types.
;;
;;    Signals an error of type type-error if PREDEFINED-RELATIONS is not a
;;    list.
;;
(defun add-predefined-relation (var attribute-types predefined-relations)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (dolist (type attribute-types)
    (unless (waql-type-p type)
      (error 'type-error :datum type :exptected-type 'waql-type)))
  (unless (listp predefined-relations)
    (error 'type-error :datum predefined-relations :expected-type 'list))
  (let ((relation-type (make-relation-type attribute-types)))
    (acons var relation-type
      (remove var predefined-relations :key #'car))))

;;
;;  Syntax:
;;
;;    PREDEFINED-RELATIONS => predefined-relations
;;
;;  Arguments and Values:
;;
;;    predefined-relations --- a list of predefined relations
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun predefined-relations ()
  *predefined-relations*)

;;
;;  Syntax:
;;
;;    CLEAR-DEFRELATION => empty-predefined-relations
;;
;;  Arguments and Values:
;;
;;    empty-predefined-relations --- a list of predefined relations.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun clear-defrelation()
  (setf *predefined-relations* (empty-predefined-relations)))

;;
;;  Syntax:
;;
;;    DEFRELATION var attribute-types form* => var
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    attribute-types --- a list of WAQL types.
;;    forms --- an implicit progn.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defmacro defrelation (var attribute-types &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf *predefined-relations*
             (add-predefined-relation ',var ',attribute-types
               *predefined-relations*)))
     (defparameter ,var ,@body)))
