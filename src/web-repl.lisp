#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql)


;;;
;;; REPL interface
;;;

(defvar +response-code-blank+ 0)
(defvar +response-code-continue+ 1)
(defvar +response-code-quit+ 2)
(defvar +response-code-output+ 3)
(defvar +response-code-error+ 4)

(hunchentoot:define-easy-handler (repl :uri "/repl") (i)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((cl-ppcre:scan +quit-command-regexp+ (trim i))
     (format nil "~A,The :quit command is not allowed in Web REPL."
                 +response-code-error+))
    (t
     (let ((response (funcall *repl-server* i)))
       (cl-pattern:match response
         (:blank
          (princ-to-string +response-code-blank+))
         (:continue
          (princ-to-string +response-code-continue+))
         (:quit
          (princ-to-string +response-code-quit+))
         ((:output message)
          (format nil "~A,~A" +response-code-output+ message))
         ((:error message)
          (format nil "~A,~A" +response-code-error+ message)))))))


;;;
;;; Hosting repl.html
;;;

(let ((uri "/repl.html")
      (path (asdf:system-relative-pathname :waql #P"static/repl.html")))
  (push (hunchentoot:create-static-file-dispatcher-and-handler uri path)
        hunchentoot:*dispatch-table*))


;;;
;;; Hosting dojo.js
;;;

(let ((uri "/dojo.js")
      (path (asdf:system-relative-pathname :waql #P"static/dojo.js")))
  (push (hunchentoot:create-static-file-dispatcher-and-handler uri path)
        hunchentoot:*dispatch-table*))


;;;
;;; Hosting repl.js
;;;

(let ((uri "/repl.js")
      (path (asdf:system-relative-pathname :waql #P"static/repl.js")))
  (push (hunchentoot:create-static-file-dispatcher-and-handler uri path)
        hunchentoot:*dispatch-table*))


;;;
;;; Starting / Stopping acceptor
;;;

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
                 :port 8080))

(defvar *repl-server*)

(defun start ()
  (setf *repl-server* (make-coroutine 'repl-server))
  (hunchentoot:start *acceptor*))

(defun stop ()
  (setf *repl-server* nil)
  (hunchentoot:stop *acceptor*))
