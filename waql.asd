#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

#|
  Author: Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage waql-asd
  (:use :cl :asdf))
(in-package :waql-asd)

(defsystem waql
  :version "0.1"
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:cl-pattern
               :optima
               :iterate
               :local-time
               :parser-combinators
               :cl-cont
               :cl-ppcre
               :hunchentoot
               :split-sequence)
  :components ((:module "src"
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
                     (:file "defrelation")
                     (:file "generic-functions")
                     (:file "type-of")
                     (:file "pattern-match")
                     (:file "compile-expression")
                     (:file "compiler")))
                   (:file "parser")
                   (:file "lang")))
                 (:module "repl"
                  :serial t
                  :components
                  ((:file "repl-server")
                   (:file "cl-repl")
                   (:file "hunchentoot")
                   (:file "web-repl")))
                 (:module "sandbox"
                  :serial t
                  :components
                  ((:file "sandbox"))))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op waql-test))))
