#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.repl.cl-repl)


;;
;; WAQL REPL on Common Lisp
;;

(defun repl-waql()
  (with-coroutine (repl-server)
    (princ ">>> ")
    (iterate:iter
      (let ((response (repl-server (read-line))))
        (cl-pattern:match response
          (:blank
           (princ ">>> "))
          (:continue
           (princ "... "))
          (:quit
           (return))
          ((:output message)
           (princ message)
           (fresh-line)
           (princ ">>> "))
          ((:error message)
           (princ message)
           (fresh-line)
           (princ ">>> "))
          (_
           (error "The response ~S is invalid." response)))))))
