#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :hunchentoot)

;;; To make Hunchentoot's acceptors and handlers processed in the same
;;; current package as calling thread's.
(defmethod start-thread ((taskmaster one-thread-per-connection-taskmaster)
                         thunk &key name)
  (let* ((package-name (package-name *package*))
         (initial-bindings `((*package* . (find-package ,package-name)))))
    (bt:make-thread thunk :name name :initial-bindings initial-bindings)))
