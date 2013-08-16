#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql)


;;;
;;; REPL server
;;;

(defun repl-trim (string)
  (left-trim (trim-after-semicolon string)))

(defun trim-after-semicolon (string)
  (let ((pos (position #\; string)))
    (if pos
        (subseq string 0 (1+ pos))
        string)))

(defun semicolon-terminated-p (string)
  (if (string/= string "")
      (char= #\; (aref string (1- (length string))))
      nil))

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
    (let ((trimed-line (repl-trim line)))
      ;; if empty line, continue
      (when (string= "" trimed-line)
        (yield :blank)
        (continue-loop))
      ;; if comment or semicolon only, continue
      (multiple-value-bind (result suffix success front)
          (parse-string* (choice1 (comment?) #\;) trimed-line :complete t)
        (declare (ignorable result suffix front))
        (when success
          (yield :blank)
          (continue-loop)))
     ;; ":" make command parsing start
     (when (starts-with #\: trimed-line)
       ;; :quit command
       (when (cl-ppcre:scan +quit-command-regexp+ (trim line))
         (coexit :quit))
       ;; :load command
       (when (cl-ppcre:scan +load-command-regexp+ (trim line))
         (cl-ppcre:register-groups-bind (filespec)
             (+load-command-regexp+ (trim line))
           (multiple-value-bind (output success) (load-in-repl filespec)
             (if success
               (yield (make-response :output output))
               (yield (make-response :error output))))
           (continue-loop)))
       ;; invalid command
       (yield (list :error
                    (format nil "invalid command: ~A" (trim line))))
       (continue-loop))
     ;; if semicolon-terminated line, evaluate and continue
     (when (semicolon-terminated-p trimed-line)
       (let ((response (handler-case-without-call/cc
                         (make-response :output
                           (eval (compile-waql (parse-waql trimed-line))))
                       (waql-parse-error (e) (make-response :error e))
                       (waql-compile-error (e) (make-response :error e))
                       (error (e) (make-response :error e)))))
         (yield response)
         (continue-loop)))
     ;; if not semicolon-terminated but valid expression,
     ;; evaluate and continue
     (destructuring-bind (result success)
         (handler-case-without-call/cc
           (list (parse-waql trimed-line :top-expr-p nil) t)
           (waql-parse-error (e) (list e nil)))
       (when success
         (let ((response (handler-case-without-call/cc
                           (make-response :output
                             (eval (compile-waql result)))
                           (waql-compile-error (e) (make-response :error e))
                           (error (e) (make-response :error e)))))
           (yield response)
           (continue-loop))))
     ;; if invalid expression, require further lines
     (let ((lines line) trimed-line)
       (repl-loop
         (yield :continue)
         (setf lines (format nil "~A~%~A" lines line)
               trimed-line (repl-trim lines))
         ;; if semicolon-terminated lines, evaluate and continue
         (when (semicolon-terminated-p trimed-line)
           (let ((response
                   (handler-case-without-call/cc
                     (make-response :output
                       (eval (compile-waql (parse-waql trimed-line))))
                     (waql-parse-error (e) (make-response :error e))
                     (waql-compile-error (e) (make-response :error e)))
                     (error (e) (make-response :error e))))
             (yield response)
             (break-loop)))
         ;; if not semicolon-terminated but valid expression,
         ;; evaluate and continue outer loop
         (destructuring-bind (result success)
             (handler-case-without-call/cc
               (list (parse-waql trimed-line :top-expr-p nil) t)
               (waql-parse-error (e) (list e nil)))
           (when success
             (let ((response
                     (handler-case-without-call/cc
                       (make-response :output
                         (eval (compile-waql result)))
                       (waql-compile-error (e) (make-response :error e))
                       (error (e) (make-response :error e)))))
               (yield response)
               (break-loop)))))))))

(defun load-waql-error-printer (condition stream)
  (princ (load-waql-error-message condition) stream))

(define-condition load-waql-error (error)
  ((output :reader load-waql-error-output
           :initarg :output
           :type string)
   (message :reader load-waql-error-message
            :initarg :message
            :type string))
  (:report load-waql-error-printer))

(define-condition load-waql-parse-error (load-waql-error) ()
  (:report load-waql-error-printer))

(define-condition load-waql-compile-error (load-waql-error) ()
  (:report load-waql-error-printer))

(define-condition load-waql-runtime-error (load-waql-error) ()
  (:report load-waql-error-printer))

(defun load-in-repl (filespec)
  (let ((output "") (code ""))
    (with-open-file (stream filespec :direction :input)
      (iterate:iter
        (iterate:for line in-file filespec using #'read-line)
        (let ((trimed-line (format nil (trim-after-semicolon line))))
          (setf code   (format nil "~A~A~%" code trimed-line)
                output (format nil "~A> ~A~%" output line))
          ;; not semicolon-terminated, continue to read next line
          (unless (semicolon-terminated-p trimed-line)
            (iterate:next-iteration))
          ;; if semicolon-terminated, evaluate and continue
          (let ((value
                  (handler-case
                      (eval (compile-waql (parse-waql code)))
                    (waql-parse-error (_)
                      (declare (ignorable _))
                      (let ((output1 (format nil "~A~A~%~%"
                                                 output "Parse error.")))
                        (return-from load-in-repl (values output1 nil))))
                    (waql-compile-error (e)
                      (let ((output1 (format nil "~A~A~%~%" output e)))
                        (return-from load-in-repl (values output1 nil))))
                    (error (e)
                      (let ((output1 (format nil "~A~A~%~%" output e)))
                        (return-from load-in-repl (values output1 nil)))))))
            (setf output (format nil "~A~A~%~%" output value))))
        (setf code "")))
    (values output t)))


;;;
;;; WAQL REPL on Common Lisp REPL
;;;

(defun repl-waql ()
  (let ((repl-server (make-coroutine 'repl-server)))
    (princ ">>> ")
    (iterate:iter
      (let ((response (funcall repl-server (read-line))))
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
           (error "Invalid response: ~A." response)))))))


