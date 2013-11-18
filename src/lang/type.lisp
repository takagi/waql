#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.type)

;;
;;  Syntax:
;;
;;    WAQL-TYPE-P object => boolean
;;
;;  Arguments and Values:
;;
;;    object --- an object.
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
(defun waql-type-p (object)
  (or (scalar-type-p object)
      (relation-type-p object)
      (function-type-p object)))

;;
;;  Syntax:
;;
;;    SCALAR-TYPE-P object => boolean
;;
;;  Arguments and Values:
;;
;;    object --- a WAQL type.
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
(defun scalar-type-p (object)
  (and (member object '(:bool :int :string :time :interval))
       t))

;;
;;  Syntax:
;;
;;    MAKE-RELATION-TYPE attr-list => relation-type
;;
;;  Arguments and Values:
;;
;;    attr-list --- a list of WAQL types.
;;    relation-type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if ATTR-LIST is not a list of
;;    WAQL types.
;;
(defun make-relation-type (attr-list)
  (dolist (attr attr-list)
    (unless (waql-type-p attr)
      (error 'type-error :datum attr :expected-type 'waql-type)))
  `(:relation ,@attr-list))

;;
;;  Syntax:
;;
;;    RELATION-TYPE-P type => boolean
;;
;;  Arguments and Values:
;;
;;    type --- a WAQL type.
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
(defun relation-type-p (type)
  (cl-pattern:match type
    ((:relation . _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    RELATION-TYPE-ATTRIBUTES type => attr-list
;;
;;  Arguments and Values:
;;
;;    type --- a WAQL type.
;;    attr-list --- a list of WAQL types.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if TYPE is not a WAQL type of
;;    relation.
;;
(defun relation-type-attributes (type)
  (cl-pattern:match type
    ((:relation . attr-list) attr-list)
    (_ (error "The value ~S is an invalid type." type))))

;;
;;  Syntax:
;;
;;    RELATION-TYPE-DIMENSION type => dimension
;;
;;  Arguments and Values:
;;
;;    type --- a WAQL type.
;;    dimension --- an non-negative integer.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Equivalent to RELATION-TYPE-ATTRIBUTES function.
;;
(defun relation-type-dimension (type)
  (length (relation-type-attributes type)))

;;
;;  Syntax:
;;
;;    MAKE-FUNCTION-TYPE arg-types return-type => function-type
;;
;;  Arguments and Values:
;;
;;    arg-types --- a list of WAQL types.
;;    return-type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if ARG-TYPES is not a list of
;;    WAQL types.
;;
;;    Signals an error of type type-error if RETURN-TYPE is not a WAQL
;;    type.
;;
(defun make-function-type (arg-types return-type)
  (dolist (arg arg-types)
    (unless (waql-type-p arg)
      (error 'type-error :datum arg :expected-type 'waql-type)))
  (unless (waql-type-p return-type)
    (error 'type-error :datum return-type :expected-type 'waql-type))
  `(:function ,arg-types ,return-type))

;;
;;  Syntax:
;;
;;    FUNCTION-TYPE-P type => boolean
;;
;;  Arguments and Values:
;;
;;    type --- a WAQL type.
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
(defun function-type-p (type)
  (cl-pattern:match type
    ((:function . _) t)
    (_ nil)))

;;
;;  Syntax:
;;
;;    FUNCTION-TYPE-ARGUMENT-TYPES type => arg-types
;;
;;  Arguments and Values:
;;
;;    type --- a WAQL type.
;;    arg-types --- a list of WAQL types.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if TYPE is a malformed WAQL
;;    type of function.
;;
;;    Signals an error of type simple-error if TYPE is not a WAQL type of
;;    function.
;;
(defun function-type-argument-types (type)
  (cl-pattern:match type
    ((:function arg-types _) arg-types)
    ((:function . _) (error "The value ~S is malformed." type))
    (_ (error "The value ~S is an invalid type." type))))

;;
;;  Syntax:
;;
;;    FUNCTION-TYPE-RETURN-TYPE type => return-type
;;
;;  Arguments and Values:
;;
;;    type --- a WAQL type.
;;    return-type --- a WAQL type.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type simple-error if TYPE is a malformed WAQL
;;    type of function.
;;
;;    Signals an error of type type-error if TYPE is not a WAQL type of
;;    function.
;;
(defun function-type-return-type (type)
  (cl-pattern:match type
    ((:function _ return-type) return-type)
    ((:function . _) (error "The value ~S is malformed." type))
    (_ (error "The value ~S is an invalid type." type))))
