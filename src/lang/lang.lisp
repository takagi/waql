#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang)

(defmacro waql (code)
  (unless (stringp code)
    (error 'type-error :datum code :expected-type 'string))
  (compile-waql (parse-waql (ensure-semicolon-terminated code))))

(defmacro waql-in-sexp (sexp)
  (compile-waql sexp))

(defun eval-waql (code)
  (eval (compile-waql (parse-waql (ensure-semicolon-terminated code)))))

(defun eval-waql-in-sexp (sexp)
  (eval (compile-waql sexp)))

(defun precompile-waql (code &rest args)
  (eval `#'(lambda ,args
             ,(compile-waql
                (parse-waql
                  (ensure-semicolon-terminated code))))))

(defun precompile-waql-in-sexp (sexp &rest args)
  (eval `#'(lambda ,args
             ,(compile-waql sexp))))
