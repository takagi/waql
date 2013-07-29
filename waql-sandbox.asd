#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage waql-sandbox-asd
  (:use :cl :asdf))
(in-package :waql-sandbox-asd)

(defsystem waql-sandbox
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:waql
               :cl-test-more)
  :components ((:module "sandbox"
                :serial t
                :components
                ((:file "package")
                 (:file "waql-sandbox"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
