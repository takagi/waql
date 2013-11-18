#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.predefined-relations)

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
;;    ADD-PREDEFINED-RELATION var attribute-types relation predefined-relations => new-predefined-relations
;;
;;  Arguments and Values:
;;
;;    var --- a WAQL symbol.
;;    attribute-types --- a list of WAQL types.
;;    relation --- a relation.
;;    predefined-relations --- a list of predefined relations.
;;
;;  Description:
;;
;;    Constructs a new list of predefined relations by adding
;;    (VAR ATTRIBUTES-TYPES RELATION) to PREDEFINED-RELATIONS. If VAR
;;    already exists, the older one will be overwritten.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if VAR is not a WAQL symbol.
;;
;;    Signals an error of type type-error if ATTRIBUTE-TYPES is not a list
;;    of WAQL types.
;;
;;    Signals an error of type type-error if RELATION is not a relation.
;;
;;    Signals an error of type type-error if PREDEFINED-RELATIONS is not a
;;    list.
;;
(defun add-predefined-relation (var attribute-types relation
                                predefined-relations)
  (unless (waql-symbol-p var)
    (error 'type-error :datum var :expected-type 'waql-symbol))
  (dolist (type attribute-types)
    (unless (waql-type-p type)
      (error 'type-error :datum type :exptected-type 'waql-type)))
  (unless (relation-p relation)
    (error 'type-error :datum relation :exptected-type 'relation))
  (unless (listp predefined-relations)
    (error 'type-error :datum predefined-relations :expected-type 'list))
  (let ((relation-type (make-relation-type attribute-types)))
    (acons var (list relation-type relation)
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
  (alexandria:with-gensyms (relation)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,relation ,@body))
         (setf *predefined-relations*
               (add-predefined-relation ',var ',attribute-types ,relation
                 *predefined-relations*))
         (defparameter ,var ,relation)
         ,relation))))
