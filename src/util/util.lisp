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
;;    result --- a function
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
;;  Srguments and Values:
;;
;;    string --- a string.
;;    result --- a string
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

