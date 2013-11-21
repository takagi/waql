#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.repl.repl-server)


;;;
;;; REPL server
;;;

(defun left-trim (string)
  (string-left-trim '(#\Space #\Tab #\Newline) string))

(defparameter +quit-command-regexp+
  "^:quit$")

(defparameter +load-command-regexp+
  "^:load\\s+(\\S+)$")

(defmacro repl-loop (&body body)
  (alexandria:with-gensyms (break-block continue-block)
    `(macrolet ((break-loop ()
                  `(return-from ,',break-block))
                (continue-loop ()
                  `(return-from ,',continue-block)))
       (block ,break-block
         (loop do
            (block ,continue-block
              ,@body))))))

(defmacro handler-case-without-call/cc (expression &rest error-clauses)
  `(cl-cont:without-call/cc
     (handler-case ,expression
       ,@error-clauses)))

(defun make-response (type expr)
  (ecase type
    (:output (list :output (princ-to-string expr)))
    (:error (list :error (princ-to-string expr)))))

(defcor repl-server (line)
  (repl-loop
    (let ((trimed-line (trim line)))
      ;; if empty line, continue
      (when (string= "" trimed-line)
        (yield :blank)
        (continue-loop))
      ;; if comment or semicolon only, continue
      (when (parse-blank-line trimed-line)
        (yield :blank)
        (continue-loop))
      ;; ":" make command parsing start
      (when (starts-with #\: trimed-line)
        ;; :quit command
        (when (scan +quit-command-regexp+ trimed-line)
          (coexit :quit))
        ;; :load command
        (when (scan +load-command-regexp+ trimed-line)
          (register-groups-bind (filespec)
              (+load-command-regexp+ trimed-line)
            (multiple-value-bind (output success) (load-in-repl filespec)
              (if success
                  (yield (make-response :output output))
                  (yield (make-response :error output))))
            (continue-loop)))
        ;; invalid command
        (yield (make-response :error
                 (format nil "The command ~A is invalid." trimed-line)))
        (continue-loop))
      ;; if semicolon-terminated line, evaluate and continue
      (when (semicolon-terminated-p trimed-line)
        (let ((response (handler-case-without-call/cc
                          (make-response :output
                            (eval (compile-waql (parse-waql trimed-line))))
                          (error (e) (make-response :error e)))))
          (yield response)
          (continue-loop)))
      ;; if not semicolon-terminated but valid expression,
      ;; evaluate and continue
      (destructuring-bind (result success)
          (handler-case-without-call/cc
            (let ((trimed-line1 (semicolon-terminated trimed-line)))
              (list (parse-waql trimed-line1) t))
            (waql-parse-error (e) (list e nil)))
        (when success
          (let ((response (handler-case-without-call/cc
                            (make-response :output
                              (eval (compile-waql result)))
                            (error (e) (make-response :error e)))))
            (yield response)
            (continue-loop))))
      ;; if invalid expression, require further lines
      (let ((lines line) trimed-line)
        (repl-loop
          (yield :continue)
          (setf lines (format nil "~A~%~A" lines line)
                trimed-line (trim lines))
          ;; if semicolon-terminated lines, evaluate and continue
          (when (semicolon-terminated-p trimed-line)
            (let ((response
                    (handler-case-without-call/cc
                      (make-response :output
                        (eval (compile-waql (parse-waql trimed-line))))
                      (error (e) (make-response :error e)))))
              (yield response)
              (break-loop)))
          ;; if not semicolon-terminated but valid expression,
          ;; evaluate and continue outer loop
          (destructuring-bind (result success)
              (handler-case-without-call/cc
                (let ((trimed-line1 (semicolon-terminated trimed-line)))
                  (list (parse-waql trimed-line1) t))
                (waql-parse-error (e) (list e nil)))
            (when success
              (let ((response
                      (handler-case-without-call/cc
                        (make-response :output
                          (eval (compile-waql result)))
                        (error (e) (make-response :error e)))))
                (yield response)
                (break-loop)))))))))

(defun load-in-repl (filespec)
  (let ((output "") (code ""))
    (handler-case
        (with-open-file (stream filespec :direction :input)
          (iterate:iter
            (iterate:for line in-file filespec using #'read-line)
            (let ((trimed-line (trim line)))
              (setf code   (format nil "~A~A~%" code trimed-line)
                    output (format nil "~A> ~A~%" output line))
              ;; not semicolon-terminated, continue to read next line
              (unless (semicolon-terminated-p trimed-line)
                (iterate:next-iteration))
              ;; if semicolon-terminated, evaluate and continue
              (let ((value
                     (handler-case
                         (eval (compile-waql (parse-waql code)))
                       (error (e)
                         (let ((output1 (format nil "~A~A~%~%" output e)))
                           (return-from load-in-repl
                             (values output1 nil)))))))
                (setf output (format nil "~A~A~%~%" output value))))
            (setf code ""))
          (values output t))
      (error (e)
        (let ((output1 (format nil "~A~A~%" output e)))
          (values output1 nil))))))
