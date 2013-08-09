#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql)


;;;
;;; Tuple
;;;

(defstruct (tuple (:constructor %make-tuple)
                  (:conc-name %tuple-)
                  (:print-object print-tuple))
  (elements nil :type list :read-only t))

(defun tuple (&rest args)
  (%make-tuple :elements args))

(defmacro with-tuple (vars tuple &body body)
  `(destructuring-bind ,vars (%tuple-elements ,tuple)
     ,@body))

(defun print-tuple (tuple stream)
  (format stream "#S~W" `(tuple ,@(%tuple-elements tuple))))


;;;
;;; Relation
;;;

(defstruct (relation (:constructor empty-relation ())
                     (:conc-name %relation-)
                     (:print-object print-relation))
  (body (make-hash-table :test #'equalp) :type hash-table :read-only t))

(defun relation->list (relation)
  ;; CAUTION: relation has no order naturally
  (hash-table-keys (%relation-body relation)))

(defun relation-member (item relation)
  (check-type item tuple)
  (let ((body (%relation-body relation)))
    (values (gethash item body))))

(defun relation-count (relation)
  (hash-table-count (%relation-body relation)))

(defun print-relation (relation stream)
  (format stream "#S~W" `(relation ,(relation-count relation)
                                   ,@(relation->list relation))))

(defun relation-adjoin (item relation)
  (check-type item tuple)
  (symbol-macrolet ((body (%relation-body relation)))
    (setf (gethash item body) t)
    relation))

(defun relation-adjoin-all (items relation)
  (reduce #'relation-adjoin items
          :initial-value relation
          :from-end t))


;;;
;;; Extending :ITERATE library for relation
;;;

;;; driver for relation

(defun %relation-elt-tuple-elements (relation index)
  (%tuple-elements (elt (relation->list relation) index)))

(defun %relation-length (relation)
  (length (relation->list relation)))

(iterate:defclause-sequence in-relation nil
  :access-fn '%relation-elt-tuple-elements
  :size-fn '%relation-length
  :sequence-type 'relation
  :element-type 'tuple
  :element-doc-string "Tuples of a relation")

(defmacro for-tuple (&rest args)
  `(for ,@args))

;;; gatherer for relation

(defmacro collect-relation (expr)
  `(iterate:reducing ,expr
                  by #'(lambda (r i)
                         (relation-adjoin i r))
       initial-value (empty-relation)))


;;;
;;; WAQL Language interface
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

(defparameter +load-command-regexp+
  "^:load\\s+(\\S+)$")

(defun repl-waql ()
  (let (line trimed-line result suffix success front sexp)
    (iterate:iter
      (princ ">>> ")
      (force-output)
      (setf line (read-line)
            trimed-line (repl-trim line))
      ;; if empty line, continue
      (when (string= "" trimed-line)
        (iterate:next-iteration))
      ;; ":" starts command parsing
      (when (starts-with #\: trimed-line)
        ;; if :quit command, exit
        (when (string= ":quit" (trim line))
          (return))
        ;; if :load command, load waql file
        (let ((%line (trim line)))
          (when (cl-ppcre:scan +load-command-regexp+ %line)
            (cl-ppcre:register-groups-bind (filespec)
                (+load-command-regexp+ %line)
              (load-waql filespec)
              (fresh-line)
              (iterate:next-iteration))))
        ;; invalid command
        (error "invalid command: ~S" (trim line)))
      ;; if comment or semilocon only, continue
      (multiple-value-setq (result suffix success front)
        (parse-string* (choice1 (comment?) #\;) trimed-line :complete t))
      (when success
        (iterate:next-iteration))
      ;; if semicolon-terminated line, evaluate and continue
      (when (semicolon-terminated-p trimed-line)
        (multiple-value-setq (result suffix success front)
          (parse-string* (expr-top*) trimed-line :complete t))
        (unless success
          (error "parse error: ~S" line))
        (setf sexp (compile-waql result))
        (princ (eval sexp))
        (fresh-line)
        (iterate:next-iteration))
      ;; if not semicolon-terminated but valid expression,
      ;; evaluate and continue
      (multiple-value-setq (result suffix success front)
        (parse-string* (expr*) trimed-line :complete t))
      (when success
        (setf sexp (compile-waql result))
        (princ (eval sexp))
        (fresh-line)
        (iterate:next-iteration))
      ;; if invalid expression, try multi line input
      (iterate:iter
        (princ "... ")
        (force-output)
        (setf line (concatenate 'string line (string #\Newline) (read-line))
              trimed-line (repl-trim line))
        ;; if semicolon-terminated line, evaluate and continue outer loop
        (when (semicolon-terminated-p line)
          (multiple-value-setq (result suffix success front)
            (parse-string* (expr-top*) trimed-line :complete t))
          (unless success
            (error "parse error: ~S" line))
          (setf sexp (compile-waql result))
          (princ (eval sexp))
          (fresh-line)
          (return))
        ;; if not semicolon-terminated but valid expression,
        ;; evaluate and continue outer loop
        (multiple-value-setq (result suffix success front)
          (parse-string* (expr*) trimed-line :complete t))
        (when success
          (setf sexp (compile-waql result))
          (princ (eval sexp))
          (fresh-line)
          (return))))))

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
;;; Evaluating WAQL
;;;

(defmacro eval-waql (expr)
  (compile-waql expr))

(defun compile-waql (expr)
  (compile-expression-top
    (specialize-function-top
      (solve-pattern-match-top expr))))


;;;
;;; Solving pattern match
;;;

(defun solve-pattern-match-top (expr)
  (solve-pattern-match expr (empty-patenv)))

(defun solve-pattern-match (expr patenv)
  (cond
    ((literal-p expr) expr)
    ((symbol-p expr) (solve-pattern-match-symbol expr))
    ((let-p expr) (solve-pattern-match-let expr patenv))
    ((query-p expr) (solve-pattern-match-query expr patenv))
    ((lisp-form-p expr) expr)
    ((function-p expr) (solve-pattern-match-function expr patenv))
    (t (error "invalid expression: ~S" expr))))


;;;
;;; Solving pattern match - Symbol
;;;

(defun solve-pattern-match-symbol (expr)
  (unless (null (percent-symbol-p expr))
    (error "symbol beginning with \"%\" is reserved: ~S" expr))
  expr)


;;;
;;; Solving pattern match - Let
;;;

(defun solve-pattern-match-let (expr patenv)
  (cond
    ((let-var-p expr) (solve-pattern-match-let-var expr patenv))
    ((let-fun-p expr) (solve-pattern-match-let-fun expr patenv))
    (t (error "invalid expression: ~S" expr))))

(defun solve-pattern-match-let-var (expr patenv)
  (let ((lvar  (let-var expr))
        (lexpr (let-expr expr))
        (lbody (let-body expr)))
    (let ((patenv1 (add-patenv lvar patenv)))
      (let ((lexpr1 (solve-pattern-match lexpr patenv))
            (lbody1 (solve-pattern-match lbody patenv1)))
        (make-let-var lvar lexpr1 lbody1)))))

(defun solve-pattern-match-let-fun (expr patenv)
  (let ((lvar      (let-var expr))
        (largs     (let-args expr))
        (larg-vars (let-arg-vars expr))
        (lexpr     (let-expr expr))
        (lbody     (let-body expr)))
    (let ((patenv1 (reduce (flip #'add-patenv) larg-vars
                           :initial-value patenv)))
      (let ((lexpr1 (solve-pattern-match lexpr patenv1))
            (lbody1 (solve-pattern-match lbody patenv)))
        (make-let-fun lvar largs lexpr1 lbody1)))))


;;;
;;; Solving pattern match - Query
;;;

(defun solve-pattern-match-query (expr patenv)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (destructuring-bind (quals1 exprs1)
        (solve-pattern-match-quals quals exprs patenv)
      (make-query exprs1 quals1))))

(defun solve-pattern-match-quals (quals exprs patenv)
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (destructuring-bind (qual1 rest1 exprs1)
            (solve-pattern-match-qual qual rest exprs patenv)
          (list (cons qual1 rest1) exprs1)))
      (let ((exprs1 (solve-pattern-match-exprs exprs patenv)))
        (list nil exprs1))))

(defun solve-pattern-match-qual (qual rest exprs patenv)
  (cond
    ((quantification-p qual)
     (solve-pattern-match-quantification qual rest exprs patenv))
    (t (solve-pattern-match-predicate qual rest exprs patenv))))

(defun solve-pattern-match-exprs (exprs patenv)
  (mapcar #'(lambda (expr)
              (solve-pattern-match expr patenv))
          exprs))


;;;
;;; Solving pattern match - Query - Quantification
;;;

(defun solve-pattern-match-quantification (qual rest exprs patenv)
  (let ((vars (quantification-vars qual))
        (rel  (quantification-relation qual)))
    (unless (notany #'percent-symbol-p vars)
      (error "symbol beginning with \"%\" is reserved: ~S" vars))
    (unless (individual-variables-p vars)
      (error "duplicated variables: ~S" vars))
    ;; do pattern matching recursively on rel
    (let ((rel1 (solve-pattern-match rel patenv)))
      ;; do main pattern matching in this quantification
      (destructuring-bind (vars1 patenv1 preds)
          (pattern-matcher-result
            (pattern-matcher-match-all vars
              (make-pattern-matcher patenv)))
        ;; do pattern matching recursively on rest and exprs
        (let ((qual1 (make-quantification vars1 rel1)))
        (destructuring-bind (rest1 exprs1)
            (solve-pattern-match-quals rest exprs patenv1)
          (list qual1 (append preds rest1) exprs1)))))))

(defun individual-variables-p (vars)
  (let ((vars1 (remove-if #'underscore-notation-p vars)))
    (equal vars1 (remove-duplicates vars1))))

(defun underscore-notation-p (var)
  (string= "_" (princ-to-string var)))


;;;
;;; Solving pattern match - Query - Predicate
;;;

(defun solve-pattern-match-predicate (pred rest exprs patenv)
  (let ((pred1 (solve-pattern-match pred patenv)))
    (destructuring-bind (rest1 exprs1)
        (solve-pattern-match-quals rest exprs patenv)
      (list pred1 rest1 exprs1))))


;;;
;;; Solving pattern match - Function application
;;;

(defun solve-pattern-match-function (expr patenv)
  (cl-pattern:match expr
    ((op . args) `(,op ,@(mapcar #'(lambda (x)
                                     (solve-pattern-match x patenv))
                                 args)))
    (_ (error "invalid expression: ~S" expr))))


;;;
;;; Solving pattern match - Pattern matcher
;;;

(defstruct (pattern-matcher (:constructor %make-pattern-matcher)
                            (:conc-name %pattern-matcher-))
  (vars   nil :type list   :read-only t)
  (patenv nil :type patenv :read-only t)
  (preds  nil :type list   :read-only t))

(defun make-pattern-matcher (patenv)
  (%make-pattern-matcher :patenv patenv))

(defmacro with-%pattern-matcher (((vars patenv preds) matcher) &body form)
  (with-gensyms (vars1 patenv1 preds1)
    `(let ((,vars   (%pattern-matcher-vars ,matcher))
           (,patenv (%pattern-matcher-patenv ,matcher))
           (,preds  (%pattern-matcher-preds ,matcher)))
       (multiple-value-bind (,vars1 ,patenv1 ,preds1) ,@form
         (%make-pattern-matcher :vars   ,vars1
                                :patenv ,patenv1
                                :preds  ,preds1)))))

(defvar *underscore-count* 1)

(defun pattern-matcher-match (var matcher)
  (assert (symbolp var))
  (with-%pattern-matcher ((vars patenv preds) matcher)
    (cond
      ((underscore-notation-p var)
       (let ((var1 (unique-symbol var *underscore-count*)))
         (incf *underscore-count*)
         (let ((vars1 (cons var1 vars)))
           (values vars1 patenv preds))))
      (t
       (cl-pattern:match (lookup-patenv var patenv)
         ((_ . count)
          (let ((var1 (unique-symbol var count)))
            (let ((vars1   (cons var1 vars))
                  (patenv1 (inc-patenv var patenv))
                  (preds1  (cons `(= ,var ,var1) preds)))
              (values vars1 patenv1 preds1))))
         (_
          (let ((vars1   (cons var vars))
                (patenv1 (add-patenv var patenv)))
            (values vars1 patenv1 preds))))))))

(defun unique-symbol (var count)
  (check-type var symbol)
  (check-type count integer)
  (let ((strs (mapcar #'princ-to-string (list "%" var count))))
    (intern (apply #'concatenate 'string strs)
            (symbol-package var))))

(defun pattern-matcher-match-all (vars matcher)
  (reduce (flip #'pattern-matcher-match)
          vars :initial-value matcher))

(defun pattern-matcher-result (matcher)
  (list (reverse (%pattern-matcher-vars matcher))
        (%pattern-matcher-patenv matcher)
        (reverse (%pattern-matcher-preds matcher))))


;;;
;;; Solving pattern match - Pattern matching environment
;;;

(defstruct (patenv (:constructor %make-patenv)
                   (:conc-name %patenv-)
                   (:print-object print-patenv))
  (elements nil :type list :read-only t))  ; alist { var -> counter }

(defun empty-patenv ()
  (%make-patenv))

(defmacro with-%patenv-elements ((elems patenv) &body form)
  `(let ((,elems (%patenv-elements ,patenv)))
     (%make-patenv :elements (progn ,@form))))

(defun add-patenv (var patenv)
  (assert (symbolp var))
  (unless (null (lookup-patenv var patenv))
    (error "variable ~S already exists" var))
  (with-%patenv-elements (elems patenv)
    (acons var 1 elems)))

(defun inc-patenv (var patenv)
  (assert (symbolp var))
  (labels ((%inc-patenv (var elems)
             (cl-pattern:match elems
               (((var1 . cnt) . rest)
                (if (eq var1 var)
                    (acons var1 (1+ cnt) rest)
                    (acons var1 cnt (%inc-patenv var rest))))
               (_ (error "variable ~S does not exist" var)))))
    (with-%patenv-elements (elems patenv)
      (%inc-patenv var elems))))

(defun lookup-patenv (var patenv)
  (assoc var (%patenv-elements patenv)))

(defun print-patenv (patenv stream)
  (format stream "#S~W" `(patenv ,@(%patenv-elements patenv))))


;;;
;;; Function specialization
;;;

(defun specialize-function-top (expr)
  (car (specialize-function expr (empty-typenv))))

(defun specialize-function (expr typenv)
  (cond
    ((literal-p expr) (specialize-function-literal expr))
    ((symbol-p expr) (specialize-function-symbol expr typenv))
    ((let-p expr) (specialize-function-let expr typenv))
    ((query-p expr) (specialize-function-query expr typenv))
    ((lisp-form-p expr) (specialize-function-lisp-form expr))
    ((function-p expr) (specialize-function-function expr typenv))
    (t (error "invalid expression: ~S" expr))))


;;;
;;; Function specialization - Literal
;;;

(defun specialize-function-literal (expr)
  (unless (literal-p expr)
    (error "invalid expression: ~S" expr))
  (list expr :int))

;;;
;;; Function specialization - Symbol
;;;

(defun specialize-function-symbol (expr typenv)
  (unless (symbol-p expr)
    (error "invalid expression: ~S" expr))
  (acond
    ((lookup-typenv expr typenv)
     (unless (not (function-type-p it))
       (error "symbol ~S is bound to function" expr))
     (list expr it))
    ((lookup-predefined-relations expr)
     (list expr it))
    (t (error "unbound variable: ~S" expr))))


;;;
;;; Function specialization - Let
;;;

(defun specialize-function-let (expr typenv)
  (cond
    ((let-var-p expr) (specialize-function-let-var expr typenv))
    ((let-fun-p expr) (specialize-function-let-fun expr typenv))
    (t (error "invalid expression: ~S" expr))))

(defun specialize-function-let-var (expr typenv)
  (let ((lvar  (let-var expr))
        (lexpr (let-expr expr))
        (lbody (let-body expr)))
    (destructuring-bind (lexpr1 var-type)
        (specialize-function lexpr typenv)
      (let ((typenv1 (add-typenv lvar var-type typenv)))
        (destructuring-bind (lbody1 type1)
            (specialize-function lbody typenv1)
          (list (make-let-var lvar lexpr1 lbody1)
                type1))))))

(defun specialize-function-let-fun (expr typenv)
  (let ((lvar       (let-var expr))
        (largs      (let-args expr))
        (larg-types (let-arg-types expr))
        (lexpr      (let-expr expr))
        (lbody      (let-body expr)))
    (let ((typenv1 (reduce #'(lambda (%typenv var-type)
                               (destructuring-bind (var type) var-type
                                 (add-typenv var type %typenv)))
                           largs :initial-value typenv)))
      (destructuring-bind (lexpr1 return-type)
          (specialize-function lexpr typenv1)
        (let* ((funtype (make-function-type larg-types return-type))
               (typenv2 (add-typenv lvar funtype typenv)))
          (destructuring-bind (lbody1 type1)
              (specialize-function lbody typenv2)
            (list (make-let-fun lvar largs lexpr1 lbody1)
                  type1)))))))


;;;
;;; Function specialization - Query
;;;

(defun specialize-function-query (expr typenv)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (destructuring-bind (quals1 exprs1 type1)
        (specialize-function-quals quals exprs typenv)
      (list (make-query exprs1 quals1) type1))))

(defun specialize-function-quals (quals exprs typenv)
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (destructuring-bind (qual1 rest1 exprs1 type1)
            (specialize-function-qual qual rest exprs typenv)
          (list (cons qual1 rest1) exprs1 type1)))
      (destructuring-bind (exprs1 type1)
          (specialize-function-exprs exprs typenv)
        (list nil exprs1 type1))))


(defun specialize-function-qual (qual rest exprs typenv)
  (cond
    ((quantification-p qual)
     (specialize-function-quantification qual rest exprs typenv))
    (t (specialize-function-predicate qual rest exprs typenv))))

(defun specialize-function-exprs (exprs typenv)
  (let ((specialized-expr-and-types
         (mapcar #'(lambda (expr)
                     (specialize-function expr typenv))
                 exprs)))
    (let ((exprs1 (mapcar #'car specialized-expr-and-types))
          (attr-types (mapcar #'cadr specialized-expr-and-types)))
      (list exprs1 (make-relation-type attr-types)))))


;;;
;;; Function specialization - Query - Quantification
;;;

(defun specialize-function-quantification (qual rest exprs typenv)
  (let ((vars (quantification-vars qual))
        (rel  (quantification-relation qual)))
    ;; error if any variables already exist in environment
    (let ((%lookup-typenv (rcurry #'lookup-typenv typenv)))
      (unless (notany %lookup-typenv vars)
        (error "variables ~S already exist in environment" vars)))
    (destructuring-bind (rel1 rel-type) (specialize-function rel typenv)
      ;; error if quantification binder is not of relation type
      (unless (relation-type-p rel-type)
        (error "quantification binder must be of relation type: ~S" rel))
      ;; error if relation dimension does not match
      (unless (= (length vars) (relation-type-dim rel-type))
        (error "variables do not match to relation dimension"))
      ;; add variables to environment and process sub expression recursively
      (let ((%add-typenv #'(lambda (%typenv var-type)
                             (destructuring-bind (var . type) var-type
                               (add-typenv var type %typenv))))
            (attr-types (relation-type-attrs rel-type)))
        (let ((typenv1 (reduce %add-typenv (mapcar #'cons vars attr-types)
                               :initial-value typenv)))
          (destructuring-bind (rest1 exprs1 type1)
              (specialize-function-quals rest exprs typenv1)
            (list (make-quantification vars rel1)
                  rest1 exprs1 type1)))))))


;;;
;;; Function specialization - Query - Predicate
;;;

(defun specialize-function-predicate (pred rest exprs typenv)
  (destructuring-bind (pred1 pred-type) (specialize-function pred typenv)
    (unless (eq pred-type :bool)
      (error "predicate must have :bool reutrn type: ~S" pred))
    (destructuring-bind (rest1 exprs1 type1)
        (specialize-function-quals rest exprs typenv)
      (list pred1 rest1 exprs1 type1))))


;;;
;;; Function specialization - Lisp form
;;;

(defun specialize-function-lisp-form (expr)
  (unless (lisp-form-p expr)
    (error "invalid expression: ~S" expr))
  ;; lisp form has no information about its type in WAQL layer,
  ;; so assume that returned type of lisp form always :bool
  (list expr :bool))


;;;
;;; Function specialization - Function application
;;;

(defun specialize-function-function (expr typenv)
  (let ((operator (function-operator expr))
        (operands (function-operands expr)))
    (cond
      ((lookup-typenv operator typenv)
       (specialize-function-function-in-typenv operator operands typenv))
      ((generic-function-p expr)
       (specialize-function-generic-function operator operands typenv))
      (t (error "undefined function: ~S" operator)))))

(defun specialize-function-function-in-typenv (operator operands typenv)
  (let ((%specialize-function (rcurry #'specialize-function typenv)))
    (let* ((pairs         (mapcar %specialize-function operands))
           (operands1     (mapcar #'car pairs))
           (operand-types (mapcar #'cadr pairs)))
      (let* ((type        (lookup-typenv operator typenv))
             (arg-types   (function-type-arg-types type))
             (return-type (function-type-return-type type)))
        (unless (function-type-p type)
          (error "symbol ~S is bound to variable" operator))
        (unless (length= arg-types operand-types)
          (error "invalid number of arguments: ~S" (length operands)))
        (unless (equal arg-types operand-types)
          (error "invalid type of arguments: ~S" `(,operator ,@operands)))
        (list (make-function operator operands1)
              return-type)))))

(defun specialize-function-generic-function (operator operands typenv)
  (let ((%specialize-function (rcurry #'specialize-function typenv)))
      (let* ((pairs         (mapcar %specialize-function operands))
             (operands1     (mapcar #'car pairs))
             (operand-types (mapcar #'cadr pairs)))
        (destructuring-bind (return-type operator1)
            (lookup-generic-function operator operand-types)
          (list (make-function operator1 operands1)
                return-type)))))


;;;
;;; Function specialization - Function table
;;;

(defparameter +function-table+
  '(=       (((:user :user)   :bool user=)
             ((:event :event) :bool event=)
             ((:int :int)     :bool =))
    <       (((:event :event) :bool event<)
             ((:int :int)     :bool <))
    count   (((:relation)     :int  relation-count))
    user    (((:int)          :user user))
    user-id (((:user)         :int  user-id))))

(defparameter +generic-functions+
  (let ((alist (plist-alist +function-table+)))
    (mapcar #'car alist)))

(defun lookup-generic-function (operator operand-types)
  (let ((candidates (getf +function-table+ operator)))
    (when candidates
      (let ((func (assoc operand-types candidates :test #'match-types-p)))
        (unless func
          (error "invalid argument types for function ~S : ~S"
                 operator operand-types))
        (cdr func)))))


;;;
;;; Function specialization - Type environment
;;;

(defun empty-typenv ()
  nil)

(defun add-typenv (var type typenv)
  (assert (symbolp var))
  (assert (type-p type))
  (acons var type typenv))

(defun lookup-typenv (var typenv)
  (assert (symbolp var))
  (cdr (assoc var typenv)))

(defun remove-typenv (var typenv)
  (assert (symbolp var))
  (remove var typenv :key #'car))


;;;
;;; Compiler
;;;

(defun compile-expression-top (expr)
  (compile-expression expr (empty-compenv) nil))

(defun compile-expression (expr compenv scope)
  (cond
    ((literal-p expr) (compile-literal expr))
    ((symbol-p expr) (compile-symbol expr compenv scope))
    ((let-p expr) (compile-let expr compenv scope))
    ((query-p expr) (compile-query expr compenv scope))
    ((lisp-form-p expr) (compile-lisp-form expr))
    ((function-p expr) (compile-function expr compenv scope))
    (t (error "invalid expression: ~S" expr))))


;;;
;;; Compiler - Literal
;;;

(defun compile-literal (expr)
  (unless (literal-p expr)
    (error "invalid expression: ~S" expr))
  expr)


;;;
;;; Compiler - Symbol
;;;

(defun compile-symbol (expr compenv scope)
  (unless (symbol-p expr)
    (error "invalid expression: ~S" expr))
  (cond
    ((lookup-compenv expr compenv)
     (compile-symbol-in-compenv expr compenv scope))
    ((lookup-predefined-relations expr)
     (compile-symbol-in-predefined-relations expr))
    (t (error "unbound variable: ~S" expr))))

(defun compile-symbol-in-compenv (expr compenv scope)
  (cl-pattern:match (lookup-compenv expr compenv)
    (:qvar
     (scoped-symbol expr scope))
    ((:argvar expr1)
     expr1)
    ((:letvar expr1 compenv1)
     (let ((scope1 (scoping-symbol expr)))
       (compile-expression expr1 compenv1 scope1)))
    ((:letfun . _)
     (error "symbol ~S is bound to function" expr))))

(defun compile-symbol-in-predefined-relations (expr)
  expr)


;;;
;;; Compiler - Let
;;;

(defun compile-let (expr compenv scope)
  (cond
    ((let-var-p expr) (compile-let-var expr compenv scope))
    ((let-fun-p expr) (compile-let-fun expr compenv scope))
    (t (error "invalid expression: ~S" expr))))

(defun compile-let-var (expr compenv scope)
  (let ((lvar  (let-var expr))
        (lexpr (let-expr expr))
        (lbody (let-body expr)))
    (let ((compenv1 (add-letvar-compenv lvar lexpr compenv)))
      (compile-expression lbody compenv1 scope))))

(defun compile-let-fun (expr compenv scope)
  (let ((lvar  (let-var expr))
        (largs (let-arg-vars expr))
        (lexpr (let-expr expr))
        (lbody (let-body expr)))
    (let ((compenv1 (add-letfun-compenv lvar largs lexpr compenv)))
      (compile-expression lbody compenv1 scope))))


;;;
;;; Compiler - Query
;;;

(defun compile-query (expr compenv scope)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (compile-query-quals quals exprs compenv scope :outermost t)))

(defun compile-query-quals (quals exprs compenv scope &key outermost)
  (assert (or (null outermost)
              (and outermost
                   (quantification-p (car quals)))))
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (compile-query-qual qual rest exprs compenv scope outermost))
      (compile-query-exprs exprs compenv scope)))

(defun compile-query-qual (qual rest exprs compenv scope outermost)
  (cond
    ((quantification-p qual)
     (compile-quantification qual rest exprs compenv scope outermost))
    (t (compile-predicate qual rest exprs compenv scope))))

(defun compile-query-exprs (exprs compenv scope)
  (let ((%compile-expression (rcurry #'compile-expression compenv scope)))
    (let ((compiled-exprs (mapcar %compile-expression exprs)))
      `(iterate:in outermost
         (collect-relation (tuple ,@compiled-exprs))))))


;;;
;;; Compiler - Query - Quantification
;;;

(defun compile-quantification (qual rest exprs compenv scope outermost)
  (let ((%scoped-symbol (rcurry #'scoped-symbol scope)))
    (let ((vars (quantification-vars qual))
          (rel  (quantification-relation qual)))
      (let ((vars1 (mapcar %scoped-symbol vars))
            (compenv1 (reduce (flip #'add-qvar-compenv) vars
                              :initial-value compenv)))
        (if outermost
            `(iterate:iter outermost
               (for-tuple ,vars1 in-relation
                          ,(compile-expression rel compenv scope))
                 ,(compile-query-quals rest exprs compenv1 scope))
            `(iterate:iter
               (for-tuple ,vars1 in-relation
                          ,(compile-expression rel compenv scope))
                 ,(compile-query-quals rest exprs compenv1 scope)))))))


;;;
;;; Compiler - Query - Predicate
;;;

(defun compile-predicate (pred rest exprs compenv scope)
  `(when ,(compile-expression pred compenv scope)
     ,(compile-query-quals rest exprs compenv scope)))


;;;
;;; Compiler - Lisp form
;;;

(defun compile-lisp-form (expr)
  (lisp-form expr))


;;;
;;; Compiler - Function application
;;;

(defun compile-function (expr compenv scope)
  (let ((operator (function-operator expr))
        (operands (function-operands expr)))
    (cond
      ((lookup-compenv operator compenv)
       (compile-function-letfun operator operands compenv scope))
      (t
       (compile-function-built-in operator operands compenv scope)))))

(defun compile-function-letfun (operator operands compenv scope)
  (let ((%compile-expression (rcurry #'compile-expression compenv scope)))
    (cl-pattern:match (lookup-compenv operator compenv)
      ((:letfun args expr compenv1)
       ;; add args and operands compiled with compenv to compenv1 as
       ;; :argvar, then compile expr1 with new compenv1 and new scope
       (let ((compiled-operands (mapcar %compile-expression operands)))
         (unless (length= args compiled-operands)
           (error "invalid number of arguments: ~S"
                  (length compiled-operands)))
         (let ((pairs (mapcar #'cons args compiled-operands)))
           (let ((compenv2
                  (reduce #'(lambda (%compenv pair)
                              (destructuring-bind (arg . operand) pair
                                (add-argvar-compenv arg operand %compenv)))
                          pairs
                          :initial-value compenv1)))
             (compile-expression expr compenv2
                                 (scoping-symbol operator))))))
      (_ (error "symbol ~S is bound to variable" operator)))))

(defun compile-function-built-in (operator operands compenv scope)
  (let ((%compile-expression (rcurry #'compile-expression compenv scope)))
    `(,operator ,@(mapcar %compile-expression operands))))


;;;
;;; Compiler - Scoping
;;;

(defvar *scoping-count* 1)

(defun scoping-symbol (symbol)
  (prog1
      (symbolicate (list "%" symbol *scoping-count*)
                   :package (symbol-package symbol))
    (incf *scoping-count*)))

(defun scoped-symbol (symbol scope)
  (if scope
      (symbolicate (list scope "." symbol))
      symbol))


;;;
;;; Compiler - Compiling environment
;;;
;;; 'elements' of compenv are:
;;;   alist { var ->  :qvar
;;;                | (:argvar expr)
;;;                | (:letvar expr compenv)
;;;                | (:letfun args expr compenv)
;;;

(defstruct (compenv (:constructor %make-compenv)
                    (:conc-name %compenv-)
                    (:print-object print-compenv))
  (elements nil :type list :read-only t))

(defun empty-compenv ()
  (%make-compenv))

(defmacro with-%compenv-elements ((elems compenv) &body form)
  `(let ((,elems (%compenv-elements ,compenv)))
     (%make-compenv :elements (progn ,@form))))

(defun add-qvar-compenv (var compenv)
  (assert (symbolp var))
  (with-%compenv-elements (elems compenv)
    (acons var :qvar elems)))

(defun add-argvar-compenv (var expr compenv)
  (assert (symbolp var))
  (with-%compenv-elements (elems compenv)
    (acons var (list :argvar expr)
           elems)))

(defun add-letvar-compenv (var expr compenv)
  (assert (symbolp var))
  (with-%compenv-elements (elems compenv)
    (acons var (list :letvar expr compenv)
           elems)))

(defun add-letfun-compenv (var args expr compenv)
  (assert (symbolp var))
  (assert (listp args))
  (with-%compenv-elements (elems compenv)
    (acons var (list :letfun args expr compenv)
           elems)))

(defun lookup-compenv (var compenv)
  (assert (symbolp var))
  (let ((elems (%compenv-elements compenv)))
    (cdr (assoc var elems))))

(defun print-compenv (compenv stream)
  (let ((elems (%compenv-elements compenv)))
    (format stream "#S~W" `(compenv ,@elems))))


;;;
;;; Syntax - Literal
;;;

(defun literal-p (expr)
  (typep expr 'fixnum))


;;;
;;; Syntax - Symbol
;;;

(defun symbol-p (expr)
  (symbolp expr))


;;;
;;; Syntax - Let
;;;

(defun make-let-var (var expr body)
  `(let (,var ,expr) ,body))

(defun make-let-fun (var args expr body)
  `(let (,var ,args ,expr) ,body))

(defun let-p (expr)
  (cl-pattern:match expr
    (('let . _) t)
    (_ nil)))

(defun let-var-p (expr)
  (cl-pattern:match expr
    (('let (_ _) _) t)
    (_ nil)))

(defun let-fun-p (expr)
  (cl-pattern:match expr
    (('let (_ _ _) _) t)
    (_ nil)))

(defun let-var (expr)
  ;; use optima instead of cl-pattern because of uncapability in this case
  (optima:match expr
    ((list 'let (list var _) _) var)
    ((list 'let (list var _ _) _) var)
    (_ (error "invalid expression~S" expr))))

(defun let-args (expr)
  (cl-pattern:match expr
    (('let (_ args _) _) args)
    (_ (error "invalid expression: ~S" expr))))

(defun let-arg-vars (expr)
  (mapcar #'car (let-args expr)))

(defun let-arg-types (expr)
  (mapcar #'cadr (let-args expr)))

(defun let-expr (expr)
  ;; use optima instead of cl-pattern because of uncapability in this case
  (optima:match expr
    ((list 'let (list _ expr1) _) expr1)
    ((list 'let (list _ _ expr1) _) expr1)
    (_ (error "invalid expression: ~S" expr))))

(defun let-body (expr)
  ;; use optima instead of cl-pattern because of uncapability in this case
  (optima:match expr
    ((list 'let (list _ _) body) body)
    ((list 'let (list _ _ _) body) body)
    (_ (error "invalid expression: ~S" expr))))


;;;
;;; Syntax - Query
;;;

(defun make-query (exprs quals)
  `(query ,exprs ,@quals))

(defun query-p (expr)
  (cl-pattern:match expr
    (('query . _) t)
    (_ nil)))

(defun query-exprs (expr)
  (cl-pattern:match expr
    (('query exprs . _) exprs)
    (_ (error "invalid expression: ~S" expr))))

(defun query-quals (expr)
  (cl-pattern:match expr
    (('query _ . quals) quals)
    (_ (error "invalid expression: ~S" expr))))


;;;
;;; Syntax - Query - Quantification
;;;

(defun make-quantification (vars rel)
  `(<- ,vars ,rel))

(defun quantification-p (qual)
  (cl-pattern:match qual
    (('<- . _) t)
    (_ nil)))

(defun quantification-vars (qual)
  (cl-pattern:match qual
    (('<- vars _)
     (unless (and (listp vars)
                  (every #'symbol-p vars))
       (error "invalid expression: ~S" qual))
     vars)
    (_ (error "invalid expression: ~S" qual))))

(defun quantification-relation (qual)
  (cl-pattern:match qual
    (('<- _ rel) rel)
    (_ (error "invalid expression: ~S" qual))))


;;;
;;; Syntax - Lisp form
;;;

(defun lisp-form-p (expr)
  (cl-pattern:match expr
    (('lisp _) t)
    (_ nil)))

(defun lisp-form (expr)
  (cl-pattern:match expr
    (('lisp form) form)
    (_ (error "invalid expression: ~S" expr))))


;;;
;;; Syntax - Function
;;;

(defun make-function (operator operands)
  `(,operator ,@operands))

(defun function-operator (expr)
  (cl-pattern:match expr
    ((operator . _) operator)
    (_ (error "invalid expression: ~S" expr))))

(defun function-operands (expr)
  (cl-pattern:match expr
    ((_ . operands) operands)
    (_ (error "invalid expression: ~S" expr))))

(defun function-p (expr)
  (and (consp expr)
       (car expr)
       t))

(defun generic-function-p (expr)
  (and (function-p expr)
       (member (car expr) +generic-functions+)
       t))


;;;
;;; Type matching
;;;

(defun match-types-p (types pattern)
  (every #'match-type-p types pattern))

(defun match-type-p (type pattern)
  (cond
    ((relation-type-pattern-p pattern) (match-relation-type-p type pattern))
    (t (eq pattern type))))

(defun match-relation-type-p (type pattern)
  (and (relation-type-pattern-p pattern)
       (relation-type-p type)
       (cond
         ((relation-type-pattern-general-p pattern) t)
         ((relation-type-pattern-wildcard-p pattern)
          (= (relation-type-pattern-dim pattern)
             (relation-type-dim type)))
         ((relation-type-pattern-strict-p pattern)
          (equal (relation-type-pattern-attrs pattern)
                 (relation-type-attrs type)))
         (t (error "must not be reached")))))


;;;
;;; Type matching - Relation type pattern
;;;

(defun relation-type-pattern-p (pattern)
  (cl-pattern:match pattern
    (:relation t)
    ((:relation)
     (error "invalid relation type pattern: ~S" pattern))
    ((:relation '_ . attrs)
     (or (every #'wildcard-p attrs)
         (error "invalid relation type pattern: ~S" pattern)))
    ((:relation . attrs)
     (or (notany #'wildcard-p attrs)
         (error "invalid relation type pattern: ~S" pattern)))
    (_ nil)))

(defun relation-type-pattern-general-p (pattern)
  (and (relation-type-pattern-p pattern)
       (cl-pattern:match pattern
         (:relation t)
         (_ nil))))

(defun relation-type-pattern-wildcard-p (pattern)
  (and (relation-type-pattern-p pattern)
       (cl-pattern:match pattern
         ((:relation '_ . _) t)
         (_ nil))))

(defun relation-type-pattern-strict-p (pattern)
  (and (relation-type-pattern-p pattern)
       (cl-pattern:match pattern
         ((:relation '_ . _) nil)
         ((:relation . _) t)
         (_ nil))))

(defun wildcard-p (symbol)
  (eq symbol '_))

(defun relation-type-pattern-attrs (pattern)
  (unless (relation-type-pattern-p pattern)
    (error "pattern ~S is not relation type pattern" pattern))
  (cl-pattern:match pattern
    (:relation (error "relation type pattern of general does not have explicit attributes: ~S" pattern))
    ((:relation . attrs) attrs)
    (_ (error "must not be reached"))))

(defun relation-type-pattern-dim (pattern)
  (length (relation-type-pattern-attrs pattern)))


;;;
;;; Type
;;;

(defun type-p (type)
  (or (scalar-type-p type)
      (relation-type-p type)
      (function-type-p type)))


;;;
;;; Type - Scalar types
;;;

(defun scalar-type-p (type)
  (and (member type '(:bool :int :user :event :action :conversion))
       t))


;;;
;;; Type - Relation type
;;;

(defun make-relation-type (types)
  (unless (every #'scalar-type-p types)
    (error "currently, relation type can have attributes of scalar type only"))
  `(:relation ,@types))

(defun relation-type-p (type)
  (cl-pattern:match type
    ((:relation _ . _) t)
    (_ nil)))

(defun relation-type-attrs (type)
  (unless (relation-type-p type)
    (error "invalid relation type: ~S" type))
  (cdr type))

(defun relation-type-dim (type)
  (length (relation-type-attrs type)))


;;;
;;; Type - Functon type
;;;

(defun make-function-type (arg-types return-type)
  (unless (every #'scalar-type-p arg-types)
    (error "invalid types: ~S" arg-types))
  (unless (scalar-type-p return-type)
    (error "invalid type: ~S" return-type))
  `(:function ,arg-types ,return-type))

(defun function-type-p (type)
  (cl-pattern:match type
    ((:function _ _) t)
    (_ nil)))

(defun function-type-arg-types (type)
  (unless (function-type-p type)
    (error "invalid function type: ~S" type))
  (cadr type))

(defun function-type-return-type (type)
  (unless (function-type-p type)
    (error "invalid function type: ~S" type))
  (caddr type))


;;;
;;; Predefined relations
;;;

(defun make-predefined-relations ()
  (empty-typenv))

(defvar *predefined-relations* (make-predefined-relations))

(defun add-predefined-relations (var types predefined-relations)
  (assert (symbolp var))
  (add-typenv var (make-relation-type types)
    (remove-typenv var predefined-relations)))

(defun lookup-predefined-relations (var &optional (predefined-relations
                                                   *predefined-relations*))
  (assert (symbolp var))
  (lookup-typenv var predefined-relations))

(defmacro defrelation (var types &body body)
  (assert (single body))
  (with-gensyms (relation)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,relation ,(car body)))
         ;; currently, does not check relation type validity, just check-type
         (check-type ,relation relation)
         (defparameter ,var ,relation))
       (setf *predefined-relations*
             (add-predefined-relations ',var ',types
                                       *predefined-relations*)))))


;;;
;;; Parser
;;;

(defun ~ws? (Q)
  ;; skip whitespaces and comments
  (named-seq? (parser-combinators:<- result Q)
              (many? (choice (whitespace?) (comment?)))
              result))

(defun ~ws* (Q)
  ;; skip whitespaces and comments
  (named-seq* (parser-combinators:<- result Q)
              (many* (choice1 (whitespace?) (comment*)))
              result))

(defun tuple? (Q)
  (bracket? (~ws? #\<)
            (sepby1? Q (~ws? #\,))
            (~ws? #\>)))

(defun tuple* (Q)
  (bracket? (~ws* #\<)
            (sepby1* Q (~ws* #\,))
            (~ws* #\>)))

(defun comment? ()
  (seq-list? "--"
             (many? (any?))
             (choice #\Newline (end?))))

(defun comment* ()
  (seq-list* "--"
             (many* (any?))
             (choice1  #\Newline (end?))))

(defun any? ()
  (sat #'graphic-char-p))

(defun expr-top? ()
  (named-seq? (many? (choice (whitespace?) (comment?)))
              (parser-combinators:<- result (expr?))
              (~ws? #\;)
              result))

(defun expr-top* ()
  (named-seq* (many* (choice1 (whitespace?) (comment*)))
              (parser-combinators:<- result (expr*))
              (~ws* #\;)
              result))

(defun expr? ()
  (choices (let?)
           (query?)
           (fexpr?)
           (literal?)))

(defun expr* ()
  (choices1 (let-*)
            (query*)
            (fexpr*)
            (literal*)))

(defun enclosed-expr? ()
  (bracket? (~ws? #\()
            (delayed? (expr?))
            (~ws? #\))))

(defun enclosed-expr* ()
  (bracket? (~ws* #\()
            (delayed? (expr*))
            (~ws* #\))))

(defun literal? ()
  (~ws? (int?)))

(defun literal* ()
  (~ws* (int*)))

(defun reserved? ()
  (choices "let" "in" "bool" "int"))

(defun reserved* ()
  (choices1 "let" "in" "bool" "int"))

(defun symbol? ()
  (~ws? (except?
          (named-seq? (parser-combinators:<- head (symbol-head?))
                      (parser-combinators:<- tail (many? (symbol-tail?)))
                      (alexandria:symbolicate
                        (string-upcase
                          (concatenate 'string
                            (cons head tail)))))
          (reserved?))))

(defun symbol* ()
  (~ws* (except?
          (named-seq* (parser-combinators:<- head (symbol-head*))
                      (parser-combinators:<- tail (many* (symbol-tail*)))
                      (alexandria:symbolicate
                        (string-upcase
                          (concatenate 'string
                            (cons head tail)))))
          (reserved*))))

(defun symbol-head? ()
  (choices (letter?)
           #\+ #\-))

(defun symbol-head* ()
  (choices1 (letter?)
            #\+ #\-))

(defun symbol-tail? ()
  (choices (alphanum?)
           #\+ #\-))

(defun symbol-tail* ()
  (choices1 (alphanum?)
            #\+ #\-))

(defun underscore? ()
  (~ws? (named-seq? #\_ '_)))

(defun underscore* ()
  (~ws* (named-seq* #\_ '_)))

(defun let? ()
  (choice (let-var?) (let-fun?)))

(defun let-* ()
  (choice1 (let-var*) (let-fun*)))

(defun let-var? ()
  (named-seq? (~ws? "let")
              (parser-combinators:<- var (symbol?))
              (~ws? ":=")
              (parser-combinators:<- expr (delayed? (expr?)))
              (~ws? "in")
              (parser-combinators:<- body (delayed? (expr?)))
              (make-let-var var expr body)))

(defun let-var* ()
  (named-seq* (~ws* "let")
              (parser-combinators:<- var (symbol*))
              (~ws* ":=")
              (parser-combinators:<- expr (delayed? (expr*)))
              (~ws* "in")
              (parser-combinators:<- body (delayed? (expr*)))
              (make-let-var var expr body)))

(defun let-fun? ()
  (named-seq? (~ws? "let")
              (parser-combinators:<- var (symbol?))
              (parser-combinators:<- largs (many1? (larg?)))
              (~ws? ":=")
              (parser-combinators:<- expr (delayed? (expr?)))
              (~ws? "in")
              (parser-combinators:<- body (delayed? (expr?)))
              (make-let-fun var largs expr body)))

(defun let-fun* ()
  (named-seq* (~ws* "let")
              (parser-combinators:<- var (symbol*))
              (parser-combinators:<- largs (many1* (larg*)))
              (~ws* ":=")
              (parser-combinators:<- expr (delayed? (expr*)))
              (~ws* "in")
              (parser-combinators:<- body (delayed? (expr*)))
              (make-let-fun var largs expr body)))

(defun larg? ()
  (named-seq? (parser-combinators:<- var (symbol?))
              (~ws? ":")
              (parser-combinators:<- type (type?))
              (list var type)))

(defun larg* ()
  (named-seq* (parser-combinators:<- var (symbol*))
              (~ws* ":")
              (parser-combinators:<- type (type*))
              (list var type)))

(defun query? ()
  (named-seq? (~ws? "{")
              (parser-combinators:<- exprs (expr-tuple?))
              (~ws? "|")
              (parser-combinators:<- quals (sepby1? (qual?) (~ws? #\,)))
              (~ws? "}")
              (make-query exprs quals)))

(defun query* ()
  (named-seq* (~ws* "{")
              (parser-combinators:<- exprs (expr-tuple*))
              (~ws* "|")
              (parser-combinators:<- quals (sepby1* (qual*) (~ws* #\,)))
              (~ws* "}")
              (make-query exprs quals)))

(defun qual? ()
  (choice (quantification?) (delayed? (expr?))))

(defun qual* ()
  (choice1 (quantification*) (delayed? (expr*))))

(defun quantification? ()
  (named-seq? (parser-combinators:<- symbols (symbol-or-underscore-tuple?))
              (~ws? "<-")
              (parser-combinators:<- rel (delayed? (expr?)))
              (make-quantification symbols rel)))

(defun quantification* ()
  (named-seq* (parser-combinators:<- symbols (symbol-or-underscore-tuple*))
              (~ws* "<-")
              (parser-combinators:<- rel (delayed? (expr*)))
              (make-quantification symbols rel)))

(defun expr-tuple? ()
  (tuple? (delayed? (expr?))))

(defun expr-tuple* ()
  (tuple* (delayed? (expr*))))

(defun symbol-or-underscore-tuple? ()
  (tuple? (choice (symbol?)
                  (underscore?))))

(defun symbol-or-underscore-tuple* ()
  (tuple* (choice1 (symbol*)
                   (underscore*))))

(defun fexpr? ()
  (choice (infix-fexpr?)
          (prefix-fexpr?)))

(defun fexpr* ()
  (choice1 (infix-fexpr*)
           (prefix-fexpr*)))

(defun infix-fexpr? ()
  (expression? (infix-aexpr?)
               `((,(comparison-op?) :left))))

(defun infix-fexpr* ()
  (expression* (infix-aexpr*)
               `((,(comparison-op*) :left))))

(def-cached-parser comparison-op?
  (choice (mdo (~ws? #\<) (result (curry #'list '<)))
          (mdo (~ws? #\=) (result (curry #'list '=)))))

(def-cached-parser comparison-op*
  (choice1 (mdo (~ws* #\<) (result (curry #'list '<)))
           (mdo (~ws* #\=) (result (curry #'list '=)))))

(defun infix-aexpr? ()
  (choices (enclosed-expr?)
           (literal?)
           (let?)
           (query?)
           (prefix-fexpr?)))

(defun infix-aexpr* ()
  (choices1 (enclosed-expr*)
            (literal*)
            (let-*)
            (query*)
            (prefix-fexpr*)))

(defun prefix-fexpr? ()
  (named-seq? (parser-combinators:<- symbol (symbol?))
              (parser-combinators:<- aexprs (many? (prefix-aexpr?)))
              (if (null aexprs)
                  symbol
                  (make-function symbol aexprs))))

(defun prefix-fexpr* ()
  (named-seq* (parser-combinators:<- symbol (symbol*))
              (parser-combinators:<- aexprs (many* (prefix-aexpr*)))
              (if (null aexprs)
                  symbol
                  (make-function symbol aexprs))))

(defun prefix-aexpr? ()
  (choices (enclosed-expr?)
           (literal?)
           (let?)
           (query?)
           (symbol?)))

(defun prefix-aexpr* ()
  (choices1 (enclosed-expr*)
            (literal*)
            (let-*)
            (query*)
            (symbol*)))

(defun type? ()
  (choices (ty-bool?)
           (ty-int?)
           (ty-relation?)))

(defun type* ()
  (choices1 (ty-bool*)
            (ty-int*)
            (ty-relation*)))

(defun ty-bool? ()
  (named-seq? (~ws? "bool") :bool))

(defun ty-bool* ()
  (named-seq* (~ws* "bool") :bool))

(defun ty-int? ()
  (named-seq? (~ws? "int") :int))

(defun ty-int* ()
  (named-seq* (~ws* "int") :int))

(defun ty-relation? ()
  (bracket? (~ws? #\{) (type-tuple?) (~ws? #\})))

(defun ty-relation* ()
  (bracket? (~ws* #\{) (type-tuple*) (~ws* #\})))

(defun type-tuple? ()
  (tuple? (delayed? (type?))))

(defun type-tuple* ()
  (tuple* (delayed? (type*))))


;;;
;;; Utilities
;;;

(defun percent-symbol-p (symbol)
  (and (symbolp symbol)
       (starts-with #\% (princ-to-string symbol))
       t))

(defun flip (function)
  (lambda (x y)
    (funcall function y x)))

(defun symbolicate (things &key package)
  (let* ((strs (mapcar #'princ-to-string things))
         (name (apply #'concatenate 'string strs)))
    (if package
        (intern name package)
        (intern name))))

(defun single (list)
  (and (listp list)
       (car list)
       (null (cdr list))))

(defun left-trim (string)
  (string-left-trim '(#\Space #\Tab #\Newline) string))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))
