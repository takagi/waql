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
  "^:load(\\s+(\\S*))?$")

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
         (yield (list :error ":load command is not suppoted"))
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

(defun load-waql (filespec)
  (with-open-file (stream filespec :direction :input)
    (let (code result suffix success front sexp)
      (iterate:iter
        (iterate:for line in-file filespec using #'read-line)
        (setf code (if (null code)
                       (trim-after-semicolon line)
                       (concatenate 'string code (string #\Newline)
                                            (trim-after-semicolon line))))
        ;; not semicolon-terminated, continue to read next line
        (unless (semicolon-terminated-p code)
          (iterate:next-iteration))
        ;; print code with ">" leaded
        (with-input-from-string (stream1 code)
          (iterate:iter
            (iterate:for line1 in-stream stream1 using #'read-line)
            (princ "> ")
            (princ line1)
            (fresh-line)))
        ;; if semicolon-terminated, evaluate and continue
        (multiple-value-setq (result suffix success front)
          (parse-string* (expr-top*) code :complete t))
        (unless (and success (null front))
          (error "parse error: ~S" code))
        (fresh-line)
        (setf sexp (compile-waql result))
        (princ (eval sexp))
        (fresh-line)
        (terpri)
        ;; reset code to null
        (setf code nil)))))


;;;
;;; WAQL REPL on Common Lisp REPL
;;;

(defun repl-waql ()
  (let ((repl-server (repl-server)))
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


