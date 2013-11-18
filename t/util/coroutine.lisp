#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.util.coroutine)

(plan nil)

;; define a coroutine using DEFCOR macro
(defcor example (who)
  (format t "First greeting to: ~A~%" who)
  (yield 1)
  (format t "Second greeting to: ~A~%" who)
  (yield 2)
  (format t "Finally greeting to: ~A~%" who)
  (coexit 3)
  (format t "No greeting to: ~A~%" who)
  (yield 4))

;; make a coroutine instance and funcall it
(let ((example (make-coroutine 'example)))
  (is (funcall example "Smith") 1)
  (is (funcall example "Johnson") 2)
  (is (funcall example "Williams") 3)
  (is (funcall example "Brown") nil))

;; error if making undefined coroutine
(is-error (make-coroutine 'foo) simple-error)

;; WITH-COROUTINE macro, call a coroutine instance
;; as if an usual function
(with-coroutine (example)
  (is (example "Smith") 1)
  (is (example "Johnson") 2)
  (is (example "Williams") 3)
  (is (example "Brown") nil))


(finalize)
