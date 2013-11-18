#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.util.coroutine)


;;;
;;; DEFCOR macro
;;;

(defmacro defcor (name args &body body)
  (assert (and (listp args)
               (or (null (car args))
                   (null (cadr args)))))
     (if (car args)
       (defcor/arg name (car args) body)
       (defcor/no-arg name body)))

(defun defcor/arg (name arg body)
  (alexandria:with-gensyms (cont)
    `(setf (get ',name 'make-coroutine)
           #'(lambda ()
               (let (,cont)
                 #'(lambda (,arg)
                     (declare (ignorable ,arg))
                     (if ,cont
                       (funcall ,cont ,arg)
                       (cl-cont:with-call/cc
                         (macrolet ((yield (&rest args)
                                      (let ((cc (gensym)))
                                        `(setf ,',arg
                                               (cl-cont:let/cc ,cc
                                                 (setf ,',cont ,cc)
                                                 (values ,@args)))))
                                    (coexit (&rest args)
                                      `(cl-cont:let/cc _
                                         (declare (ignorable _))
                                         (setf ,',cont
                                               #'(lambda (_)
                                                   (declare (ignorable _))
                                                   (values)))
                                         (values ,@args))))
                           ,@body
                           (coexit nil))))))))))

(defun defcor/no-arg (name body)
  (alexandria:with-gensyms (cont)
    `(setf (get ',name 'make-coroutine)
           #'(lambda ()
               (let (,cont)
                 #'(lambda ()
                     (if ,cont
                       (funcall ,cont)
                       (cl-cont:with-call/cc
                         (macrolet ((yield (&rest args)
                                      (let ((cc (gensym)))
                                        `(cl-cont:let/cc ,cc
                                           (setf ,',cont ,cc)
                                           (values ,@args))))
                                    (coexit (&rest args)
                                      `(cl-cont:let/cc _
                                         (declare (ignorable _))
                                         (setf ,',cont
                                               #'(lambda (_)
                                                   (declare (ignorable _))
                                                   (values)))
                                         (values ,@args))))
                           ,@body
                           (coexit nil))))))))))


;;;
;;; MAKE-COROUTINE function
;;;

(defun make-coroutine (name)
  (let ((func (get name 'make-coroutine)))
    (unless func
      (error "The coroutine ~S is undefined." name))
    (funcall func)))


;;;
;;; WITH-GENERATOR macro
;;;

(defmacro with-coroutine ((name) &body body)
  (alexandria:with-gensyms (coroutine)
    `(let ((,coroutine (make-coroutine ',name)))
       (flet ((,name (&rest args)
                 (apply ,coroutine args)))
         ,@body))))
