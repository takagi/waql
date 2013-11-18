#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang)

(defmacro waql (code)
  (compile-waql (parse-waql code)))

(defmacro waql-in-sexp (expr)
  (compile-waql expr))
