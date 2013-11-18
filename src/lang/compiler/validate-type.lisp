#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler.validate-type)

;;
;;  Syntax:
;;
;;    VALIDATE-TYPE-TOP expr => new-expr
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
(defun validate-type-top (expr)
  (let ((typenv (reduce #'(lambda (typenv item)
                            (destructuring-bind (var type _) item
                              (declare (ignore _))
                              (add-letvar-typenv var type typenv)))
                        (predefined-relations)
                        :initial-value (empty-typenv))))
    (validate-type expr typenv)))

;;
;;  Syntax:
;;
;;    VALIDATE-TYPE expr typenv => new-expr
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
(defun validate-type (expr typenv)
  (type-of-expression expr typenv)      ; the result will be discarded
  (copy-tree expr))
