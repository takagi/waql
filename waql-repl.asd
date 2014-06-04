#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage waql-repl-asd
  (:use :cl :asdf))
(in-package :waql-repl-asd)

(defsystem waql-repl
  :version "0.1"
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:hunchentoot
               :cl-ppcre
               :cl-coroutine)
  :components ((:module "repl"
                :serial t
                :components
                ((:file "repl-server")
                 (:file "cl-repl")
                 (:file "hunchentoot")
                 (:file "web-repl"))))
  :description ""
  :long-description ""
  :in-order-to ((test-op (load-op waql-repl-test))))
