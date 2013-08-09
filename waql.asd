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
  :version "0.1-SNAPSHOT"
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:alexandria :iterate :cl-pattern :optima :anaphora
               :parser-combinators)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "data")
                 (:file "waql"))))
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
