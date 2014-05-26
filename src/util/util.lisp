#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.util)

;;
;;  Syntax:
;;
;;    SINGLE list => boolean
;;
;;  Arguments and Values:
;;
;;    list --- a list.
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
(defun single (list)
  (and (consp list)
       (null (cdr list))))

;;
;;  Syntax:
;;
;;    MINIMIZE list &key key test => result-list
;;
;;  Arguments and Values:
;;
;;    list --- a list.
;;    key --- a designator for a function of one argument.
;;    test --- a designator for a function of two arguments that returns a generalized boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if LIST is not a list.
;;
(defun minimize (list &key (key #'identity) (test #'<))
  (if list
      (destructuring-bind (top . rest) list
        (do ((nlist rest (cdr nlist))
             (result top))
            ((null nlist) (return result))
          (let ((lhs (funcall key (car nlist)))
                (rhs (funcall key result)))
            (if (funcall test lhs rhs) (setq result (car nlist))))))
      nil))

;;
;;  Syntax:
;;
;;    FLIP function => result
;;
;;  Arguments and Values:
;;
;;    function --- a designator for a function with two arguments.
;;    result --- a function.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun flip (function)
  (lambda (x y)
    (funcall function y x)))


;;
;;  Syntax:
;;
;;    TRIM string => result
;;
;;  Arguments and Values:
;;
;;    string --- a string.
;;    result --- a string.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun trim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))


;;
;;  Syntax:
;;
;;    LEFT-TRIM string => result
;;
;;  Arguments and Values:
;;
;;    string --- a string.
;;    result --- a string.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun left-trim (string)
  (string-left-trim '(#\Space #\Tab #\Newline) string))


;;
;;  Syntax:
;;
;;    RIGHT-TRIM string => result
;;
;;  Arguments and Values:
;;
;;    string --- a string.
;;    result --- a string.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun right-trim (string)
  (string-right-trim '(#\Space #\Tab #\Newline) string))


;;  Syntax:
;;
;;    SEMICOLON-TERMINATED string => result
;;
;;  Arguments and Values:
;;
;;    string --- a string.
;;    result --- a string.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if STRING is not a string.
;;
(defun semicolon-terminated (string)
  (concatenate 'string string ";"))  


;;
;;  Syntax:
;;
;;    SEMICOLON-TERMINATED-P string => boolean
;;
;;  Arguments and Values:
;;
;;    string --- a string.
;;    boolean --- a generalized boolean.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if STRING is not a string
;;
(defun semicolon-terminated-p (string)
  (unless (stringp string)
    (error 'type-error :datum string :expected-type string))
  (ends-with #\; string))


;;
;;  Syntax:
;;
;;    ENSURE-SEMICOLON-TERMINATED string => result, modified-p
;;
;;  Arguments and Values:
;;
;;    string --- a string.
;;    result --- a string.
;;    modified-p --- a generalized boolean.
;;
;;  Description:
;;
;;    If STRING ends with a semicolon, STRING is returned with
;;    MODIFIED-P of nil. Otherwise, STRING is returned being appended a
;;    semicolon, with MODIFIED-P of t.
;;
;;  Exceptional Situations:
;;
;;    Signals an error of type type-error if STRING is not a string
;;
(defun ensure-semicolon-terminated (string)
  (if (semicolon-terminated-p string)
      (values string nil)
      (values (semicolon-terminated string) t)))

;;
;;  Syntax:
;;
;;    MAPTREE fn list => result
;;
;;  Arguments and Values:
;;
;;    fn --- a designator for a function that must take one argument
;;    list --- a proper list
;;    result --- a list
;;
(defun maptree (fn list)
  (cond ((null list) nil)
        ((atom list) (funcall fn list))
        (t (cons (maptree fn (car list))
                 (maptree fn (cdr list))))))
