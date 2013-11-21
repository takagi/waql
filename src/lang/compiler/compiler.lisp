#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.compiler)

;;
;;  Syntax:
;;
;;    COMPILE-WAQL expr => form
;;
;;  Arguments and Values:
;;
;;    expr --- a WAQL expression.
;;    form --- a form.
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situations:
;;
;;    None.
;;
(defun compile-waql (expr)
  (when expr
    (compile-expression-top
     (pattern-match-expression-top expr))))
