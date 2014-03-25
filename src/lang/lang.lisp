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

(defun eval-waql (code &rest args)
  (eval `(let ,(args->bindings args)
           ,(compile-waql
              (parse-waql
                 (ensure-semicolon-terminated code))))))

(defun eval-waql-in-sexp (sexp &rest args)
  (eval `(let ,(args->bindings args)
           ,(compile-waql sexp))))

(defun args->bindings (args)
  (remove-if #'single (group args 2)))

(defun precompile-waql (code &rest args)
  (eval `#'(lambda ,args
             ,(compile-waql
                (parse-waql
                  (ensure-semicolon-terminated code))))))

(defun precompile-waql-in-sexp (sexp &rest args)
  (eval `#'(lambda ,args
             ,(compile-waql sexp))))
