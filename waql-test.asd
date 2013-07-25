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
                ((:file "package")
                 (:file "test-waql"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
