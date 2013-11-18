#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.generic-functions)

(defparameter +generic-functions+
  '(=       (((:int :int)       :bool     =)
             ((:string :string) :bool     string=)
             ((:time :time)     :bool     timestamp=))
    +       (((:time :interval) :time     time+))
    -       (((:time :interval) :time     time-))
    <       (((:int :int)       :bool     <)
             ((:time :time)     :bool     timestamp<))
    <=      (((:int :int)       :bool     <=)
             ((:time :time)     :bool     timestamp<=))
    >       (((:int :int)       :bool     >)
             ((:time :time)     :bool     timestamp>))
    >=      (((:int :int)       :bool     >=)
             ((:time :time)     :bool     timestamp>=))
    count   (((:relation)       :int      relation-count))
    exists  (((:relation)       :bool     relation-exists))
    days    (((:int)            :interval days))))

;;
;;  Syntax:
;;
;;    GENERIC-FUNCTION-P symbol => boolean
;;
;;  Arguments and Values:
;;
;;    symbol --- a symbol.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if SYMBOL is not a symbol.
;;
(defun generic-function-p (symbol)
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  (and (getf +generic-functions+ symbol)
       t))

;;
;;  Syntax:
;;
;;    SPECIALIZE-FUNCTION function-name type-list => specialized-function
;;
;;  Arguments and Values:
;;
;;    function-name --- a WAQL symbol.
;;    type-list --- a list of WAQL types.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if FUNCTION-NAME is not a WAQL
;;    symbol.
;;
;;    Signals an error of type simple-error if FUNCTION-NAME does not exist
;;    in the generic functions.
;;
;;    Signals an error of type simple-error if any entries corresponding to
;;    TYPE-LIST does not exist in the generic functions.
;;
(defun specialized-function (function-name type-list)
  (let ((candidates (getf +generic-functions+ function-name)))
    (unless candidates
      (error "The function ~S is undefined." function-name))
    (let ((func (assoc type-list candidates
                       :test #'match-type-pattern)))
      (unless func
        (error "The arguments of the function ~S do not match."
               function-name))
      (destructuring-bind (return-type specialized-name) (cdr func)
        (make-specialized-function specialized-name function-name
                                   type-list return-type)))))

;;
;;  Syntax:
;;
;;    MAKE-SPECIALIZED-FUNCTION name generic-name argument-types return-type => specialized-function
;;
;;  Arguments and Values:
;;
;;    name --- a WAQL symbol.
;;    generic-name --- a WAQL symbol.
;;    argument-types --- a list of WAQL types.
;;    return-type --- a WAQL type.
;;    specialized-function --- a specialized function.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defstruct (specialized-function (:constructor %make-specialized-function))
  (name :name :read-only t)
  (generic-name :generic-name :read-only t)
  (argument-types :argument-types :read-only t)
  (return-type :return-type :read-only t))

(defun make-specialized-function (name generic-name argument-types
                                  return-type)
  (%make-specialized-function :name name
                              :generic-name generic-name
                              :argument-types argument-types
                              :return-type return-type))

;;
;;  Syntax:
;;
;;    MATCH-TYPE-PATTERN type-list pattern-list => boolean
;;
;;  Arguments and Values:
;;
;;    type-list --- a list of WAQL types.
;;    pattern-list --- a list of WAQL type patterns.
;;    boolean --- a boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if TYPE-LIST is not a list.
;;
;;    Signals an error of type type-error if PATTERN-LIST is not a list.
;;
;;    Signals an error of type simple-error if the lenghs of TYPE-LIST and
;;    PATTERN-LIST are different.
;;
(defun match-type-pattern (type-list pattern-list)
  (unless (length= type-list pattern-list)
    (error "The lengths of ~S and ~S are different."
           type-list pattern-list))
  (every #'match-type-pattern1 type-list pattern-list))

(defun match-type-pattern1 (type pattern)
  (or (match-relation-type-pattern type pattern)
      (match-scalar-type-pattern type pattern)))

(defun match-relation-type-pattern (type pattern)
  (and (eq pattern :relation)
       (relation-type-p type)))

(defun match-scalar-type-pattern (type pattern)
  (and (scalar-type-p type)
       (eq type pattern)))
