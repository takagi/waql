#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage waql-test-asd
  (:use :cl :asdf))
(in-package :waql-test-asd)

(defsystem waql-test
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:waql
               :cl-test-more)
  :components ((:module "t"
                :serial t
                :components
                ((:file "packages")
                 (:module "util"
                  :serial t
                  :components
                  ((:file "util")
                   (:file "coroutine")))
                 (:module "lang"
                  :serial t
                  :components
                  ((:file "data")
                   (:file "type")
                   (:file "syntax")
                   (:module "compiler"
                    :serial t
                    :components
                    ((:file "patenv")
                     (:file "typenv")
                     (:file "compenv")
                     (:file "predefined-relations")
                     (:file "generic-functions")
                     (:file "type-of")
                     (:file "pattern-match")
                     (:file "validate-type")
                     (:file "specialize-function")
                     (:file "compile-expression")
                     (:file "compiler")))
                   (:file "parser")
                   (:file "lang"))))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
