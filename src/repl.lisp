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

(defmacro with-parse-string ((result suffix success front)
                             (parser string &key complete)
                             &body body)
  `(multiple-value-bind (,result ,suffix ,success ,front)
       (parse-string* ,parser ,string :complete ,complete)
     (declare (ignorable ,result ,suffix ,success ,front))
     ,@body))

(defmacro when-parse-string ((result suffix success front)
                             (parser string &key complete)
                             &body body)
  `(with-parse-string (,result ,suffix ,success ,front)
       (,parser ,string :complete ,complete)
     (when ,success
       ,@body)))

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
      (when-parse-string (result suffix success front)
          ((choice1 (comment?) #\;) trimed-line :complete t)
        (yield :blank)
        (continue-loop))
     ;; ":" make command parsing start
     (when (starts-with #\: trimed-line)
       ;; :quit command
       (when (cl-ppcre:scan +quit-command-regexp+ (trim line))
         (coexit :quit))
       ;; :load command
       (when (cl-ppcre:scan +load-command-regexp+ (trim line))
         (cl-ppcre:register-groups-bind (filespec)
             (+load-command-regexp+ (trim line))
           (let ((response (handler-case-without-call/cc
                             (let ((output (load-waql filespec)))
                               (make-respconse :output output))
                             (error (e) (make-response :error e)))))
             (yield response)))
         (continue-loop))
       ;; invalid command
       (yield (list :error
                    (format nil "invalid command: ~A" (trim line))))
       (continue-loop))
     ;; if semicolon-terminated line, evaluate and continue
     (when (semicolon-terminated-p trimed-line)
       (with-parse-string (result suffix success front)
           ((expr-top*) trimed-line :complete t)
         (if success
             (let ((response (handler-case-without-call/cc
                               (let ((sexp (compile-waql result)))
                                 (make-response :output (eval sexp)))
                               (error (e) (make-response :error e)))))
               (yield response)
               (continue-loop))
             (progn
               (yield (make-response :error
                                     (format nil "parse error: ~A" line)))
               (continue-loop)))))
     ;; if not semicolon-terminated but valid expression,
     ;; evaluate and continue
     (when-parse-string (result suffix success front)
         ((expr*) trimed-line :complete t)
       (let ((response (handler-case-without-call/cc
                         (let ((sexp (compile-waql result)))
                           (make-response :output (eval sexp)))
                         (error (e) (make-response :error e)))))
         (yield response)
         (continue-loop)))
     ;; if invalid expression, require further lines
     (let ((lines line) trimed-line)
       (repl-loop
         (yield :continue)
         (setf lines (concatenate 'string lines (string #\Newline) line)
               trimed-line (repl-trim lines))
         ;; if semicolon-terminated lines, evaluate and continue
         (when (semicolon-terminated-p trimed-line)
           (with-parse-string (result suffix success front)
               ((expr-top*) trimed-line :complete t)
             (if success
                 (let ((response (handler-case-without-call/cc
                                   (let ((sexp (compile-waql result)))
                                     (make-response :output (eval sexp)))
                                   (error (e) (make-response :error e)))))
                   (yield response)
                   (break-loop))
                 (progn
                   (yield (make-response :error
                                         (format nil "parse error: ~A" lines)))
                   (break-loop)))))
       ;; if not semicolon-terminated but valid expression,
       ;; evaluate and continue outer loop
       (when-parse-string (result suffix success front)
           ((expr*) trimed-line :complete t)
         (let ((response (handler-case-without-call/cc
                           (let ((sexp (compile-waql result)))
                             (make-response :output (eval sexp)))
                           (error (e) (make-response :error e)))))
           (yield response)
           (break-loop))))))))

(defun load-waql-error-printer (condition stream)
  (princ (load-waql-message condition) stream))

(define-condition load-waql-error (error)
  ((output :reader load-waql-output
           :initarg :output
           :type string)
   (message :reader load-waql-message
            :initarg :message
            :type string))
  (:report load-waql-error-printer))

(define-condition load-waql-parse-error (load-waql-error) ()
  (:report load-waql-error-printer))

(define-condition load-waql-compile-error (load-waql-error) ()
  (:report load-waql-error-printer))

(define-condition load-waql-runtime-error (load-waql-error) ()
  (:report load-waql-error-printer))

(defun load-waql (filespec)
  (let ((output "") (code ""))
    (with-open-file (stream filespec :direction :input)
      (iterate:iter
        (iterate:for line in-file filespec using #'read-line)
        (let ((trimed-line (format nil (trim-after-semicolon line))))
          (setf code   (concatenate 'string code
                                            (format nil "~A~%" trimed-line))
                output (concatenate 'string output
                                            (format nil "> ~A~%" line)))
          ;; not semicolon-terminated, continue to read next line
          (unless (semicolon-terminated-p trimed-line)
            (iterate:next-iteration))
          ;; if semicolon-terminated, evaluate and continue
          (multiple-value-bind (result suffix success front)
              (parse-string* (expr-top*) code :complete t)
            (declare (ignorable suffix))
            (unless (and success (null front))
              (let ((message (format nil "Parse error: ~A" code)))
                (error 'load-waql-parse-error
                       :output output :message message)))
            (let* ((sexp (handler-case (compile-waql result)
                           (simple-error (e)
                             (error 'load-waql-compile-error
                                    :output output :message (princ e)))))
                   (value (handler-case (eval sexp)
                            (error (e)
                              (error 'load-waql-runtime-error
                                     :output output :message (princ e))))))
              (setf output (concatenate 'string output
                                        (format nil "~A~%~%" value))))))
        (setf code "")))
    output))


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


