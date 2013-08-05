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
  (alexandria:hash-table-keys (%relation-body relation)))

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
;;; Evaluating WAQL
;;;

(defmacro eval-waql (expr)
  (compile-waql expr))

(defun compile-waql (expr)
  (compile-expression
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
;;     ((tuple-p expr) expr)
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
  (unless (null (lookup-patenv var patenv))
    (error "variable ~S already exists" var))
  (with-%patenv-elements (elems patenv)
    (acons var 1 elems)))

(defun inc-patenv (var patenv)
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
  (alexandria:with-gensyms (vars1 patenv1 preds1)
    `(let ((,vars   (%pattern-matcher-vars ,matcher))
           (,patenv (%pattern-matcher-patenv ,matcher))
           (,preds  (%pattern-matcher-preds ,matcher)))
       (multiple-value-bind (,vars1 ,patenv1 ,preds1) ,@form
         (%make-pattern-matcher :vars   ,vars1
                                :patenv ,patenv1
                                :preds  ,preds1)))))

(defvar *underscore-count* 1)

(defun pattern-matcher-match (var matcher)
  (with-%pattern-matcher ((vars patenv preds) matcher)
    (cond
      ((underscore-notation-p var)
       (let ((var1 (pattern-matcher-symbol var *underscore-count*)))
         (incf *underscore-count*)
         (let ((vars1 (cons var1 vars)))
           (values vars1 patenv preds))))
      (t
       (cl-pattern:match (lookup-patenv var patenv)
         ((_ . count)
          (let ((var1 (pattern-matcher-symbol var count)))
            (let ((vars1   (cons var1 vars))
                  (patenv1 (inc-patenv var patenv))
                  (preds1  (cons `(= ,var ,var1) preds)))
              (values vars1 patenv1 preds1))))
         (_
          (let ((vars1   (cons var vars))
                (patenv1 (add-patenv var patenv)))
            (values vars1 patenv1 preds))))))))

(defun pattern-matcher-symbol (var count)
  (check-type var symbol)
  (check-type count integer)
  (let ((strs (mapcar #'princ-to-string (list "%" var count))))
    (intern (apply #'concatenate 'string strs)
            (symbol-package var))))

(defun pattern-matcher-match-all (vars matcher)
  (reduce #'(lambda (matcher var)
              (pattern-matcher-match var matcher))
          vars :initial-value matcher))

(defun pattern-matcher-result (matcher)
  (list (reverse (%pattern-matcher-vars matcher))
        (%pattern-matcher-patenv matcher)
        (reverse (%pattern-matcher-preds matcher))))


;;;
;;; Function specialization
;;;

(defun specialize-function-top (expr)
  (car (specialize-function expr (empty-typenv))))

(defun specialize-function (expr typenv)
  (cond
    ((literal-p expr) (specialize-function-literal expr))
    ((symbol-p expr) (specialize-function-symbol expr typenv))
    ;; ((tuple-p expr) (specialize-function-tuple expr typenv))
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
  (let ((type (or (lookup-typenv expr typenv)
                  (lookup-typenv expr *predefined-relation-typenv*)
                  (error "unbound variable: ~S" expr))))
    (list expr type)))


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
    (destructuring-bind (rel1 rel-type)
        (specialize-function rel typenv)
      (let ((attr-types (relation-type-attrs rel-type)))
        (unless (= (length vars) (length attr-types))
          (error "variables do not match to type of relation: ~S" qual))
        (unless (notany #'(lambda (var)
                            (lookup-typenv var typenv)) vars)
          (error "variables ~S already exist in type environment" vars))
        (let ((typenv1 (reduce #'(lambda (%typenv var-type)
                                   (destructuring-bind (var . type) var-type
                                     (add-typenv var type %typenv)))
                               (mapcar #'cons vars attr-types)
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
    (let ((specialized-operand-and-types
           (mapcar #'(lambda (operand)
                       (specialize-function operand typenv))
                   operands)))
      (let ((operands1 (mapcar #'car specialized-operand-and-types))
            (operand-types (mapcar #'cadr specialized-operand-and-types)))
        (destructuring-bind (return-type operator1)
            (lookup-generic-function operator operand-types)
          (list (make-function operator1 operands1) return-type))))))


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
    user-id (((:user)         :int  user-id))))

(defparameter +generic-functions+
  (let ((alist (alexandria:plist-alist +function-table+)))
    (mapcar #'car alist)))

(defun lookup-generic-function (operator operand-types)
  (let ((candidates (getf +function-table+ operator)))
    (unless candidates
      (error "undefined function: ~S" operator))
    (let ((func (assoc operand-types candidates :test #'match-types-p)))
      (unless func
        (error "invalid argument types for function ~S : ~S"
               operator operand-types))
      (cdr func))))


;;;
;;; Function specialization - Type environment
;;;

(defun empty-typenv ()
  nil)

(defun add-typenv (var type typenv)
  (acons var type typenv))

(defun lookup-typenv (var typenv)
  (cdr (assoc var typenv)))

(defun remove-typenv (var typenv)
  (remove var typenv :key #'car))


;;;
;;; Function specialization - Predefined relation type environment
;;;

(defvar *predefined-relation-typenv* (empty-typenv))

(defun lookup-predefined-relations (var)
  (lookup-typenv var *predefined-relation-typenv*))

(defmacro defrelation (var attr-types &body body)
  ;; currently does not check type of tuples
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,var
       ,@body)
     (setf *predefined-relation-typenv*
           (add-typenv ',var '(:relation ,@attr-types)
             (remove-typenv ',var
               *predefined-relation-typenv*)))))


;;;
;;; Function specialization - Type matching
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
;;; Function specialization - Type patterns - Relation type pattern
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
;;; Function specialization - Types - scalar types
;;;

(defun scalar-type-p (type)
  (and (member type '(:int :user :event :action :conversion))
       t))


;;;
;;; Function specialization - Types - relation type
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
;;; Compiler
;;;

(defun compile-expression (expr)
  (cond
    ((literal-p expr) (compile-literal expr))
    ((symbol-p expr) (compile-symbol expr))
;;     ((tuple-p expr) (compile-tuple expr))
    ((query-p expr) (compile-query expr))
    ((lisp-form-p expr) (compile-lisp-form expr))
    ((specialized-function-p expr) (compile-function expr))
    (t (error "invalid expression: ~S" expr))))

(defun %compile-expression (expr compenv scope)
  (cond
    ((literal-p expr) (compile-literal expr))
    ((symbol-p expr) (%compile-symbol expr compenv scope))
    ((let-p expr) (%compile-let expr compenv scope))
    ;; ((tuple-p expr) (%compile-tuple expr compenv scope))
    ((query-p expr) (%compile-query expr compenv scope))
    ((lisp-form-p expr) (compile-lisp-form expr))
    ((%function-p expr) (%compile-function expr compenv scope))
    (t (error "invalid expression: ~S" expr))))


;;;
;;; Compiler - Literal
;;;

(defun literal-p (expr)
  (typep expr 'fixnum))

(defun compile-literal (expr)
  (unless (literal-p expr)
    (error "invalid expression: ~S" expr))
  expr)


;;;
;;; Compiler - Symbol
;;;

(defun symbol-p (expr)
  (symbolp expr))

(defun compile-symbol (expr)
  (unless (symbol-p expr)
    (error "invalid expression: ~S" expr))
  expr)

(defun %compile-symbol (expr compenv scope)
  (unless (symbol-p expr)
    (error "invalid expression: ~S" expr))
  (cl-pattern:match (lookup-compenv expr compenv)
    (:qvar
     (scoped-symbol expr scope))
    ((:argvar expr1)
     expr1)
    ((:letvar lexpr lcompenv)
     (%compile-expression lexpr lcompenv (scoping-symbol expr)))
    ((:letfun . _)
     (error "symbol ~S is bound to function" expr))
    (_ (if (lookup-predefined-relations expr)
           expr
           (error "unbound variable: ~S" expr)))))


;;;
;;; Compiler - Let
;;;

(defun let-p (expr)
  (or (let-var-p expr)
      (let-fun-p expr)))

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

(defun %compile-let (expr compenv scope)
  (cond
    ((let-var-p expr) (%compile-let-var expr compenv scope))
    ((let-fun-p expr) (%compile-let-fun expr compenv scope))
    (t (error "must not be reached"))))

(defun %compile-let-var (expr compenv scope)
  (let ((lvar  (let-var expr))
        (lexpr (let-expr expr))
        (lbody (let-body expr)))
    (let ((compenv1 (add-letvar-compenv lvar lexpr compenv)))
      (%compile-expression lbody compenv1 scope))))

(defun %compile-let-fun (expr compenv scope)
  (let ((lvar  (let-var expr))
        (largs (let-args expr))
        (lexpr (let-expr expr))
        (lbody (let-body expr)))
    (let ((compenv1 (add-letfun-compenv lvar largs lexpr compenv)))
      (%compile-expression lbody compenv1 scope))))


;;;
;;; Compiler - Query
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

(defun compile-query (expr)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (compile-query-quals quals exprs :outermost t)))

(defun %compile-query (expr compenv scope)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (%compile-query-quals quals exprs compenv scope :outermost t)))

(defun compile-query-quals (quals exprs &key outermost)
  (assert (or (null outermost)
              (and outermost
                   (quantification-p (car quals)))))
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (compile-query-qual qual rest exprs outermost))
      (compile-query-exprs exprs)))

(defun %compile-query-quals (quals exprs compenv scope &key outermost)
  (assert (or (null outermost)
              (and outermost
                   (quantification-p (car quals)))))
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (%compile-query-qual qual rest exprs compenv scope outermost))
      (%compile-query-exprs exprs compenv scope)))

(defun compile-query-qual (qual rest exprs outermost)
  (cond
    ((quantification-p qual)
     (compile-quantification qual rest exprs outermost))
    (t (compile-predicate qual rest exprs))))

(defun %compile-query-qual (qual rest exprs compenv scope outermost)
  (cond
    ((quantification-p qual)
     (%compile-quantification qual rest exprs compenv scope outermost))
    (t (%compile-predicate qual rest exprs compenv scope))))

(defun compile-query-exprs (exprs)
  (let ((compiled-exprs (mapcar #'compile-expression exprs)))
    `(iterate:in outermost
       (collect-relation (tuple ,@compiled-exprs)))))

(defun %compile-query-exprs (exprs compenv scope)
  (let ((%compile-expression (alexandria:rcurry #'%compile-expression
                                                 compenv scope)))
    (let ((compiled-exprs (mapcar %compile-expression exprs)))
      `(iterate:in outermost
         (collect-relation (tuple ,@compiled-exprs))))))


;;;
;;; Compiler - Query - Quantification
;;;

(defun make-quantification (vars rel)
  `(<- ,vars ,rel))

(defun quantification-p (qual)
  (cl-pattern:match qual
    (('<- . _) t)
    (_ nil)))

(defun quantification-vars (qual)
  (cl-pattern:match qual
    (('<- vars _) (unless (listp vars)
                    (error "quantification variables must be list: ~S" vars))
                  vars)
    (_ (error "invalid expression: ~S" qual))))

(defun quantification-relation (qual)
  (cl-pattern:match qual
    (('<- _ rel) rel)
    (_ (error "invalid expression: ~S" qual))))


(defun compile-quantification (qual rest exprs outermost)
  (let ((vars (quantification-vars qual))
        (rel  (quantification-relation qual)))
    (if outermost
        `(iterate:iter outermost
           (for-tuple ,vars in-relation ,(compile-expression rel))
             ,(compile-query-quals rest exprs))
        `(iterate:iter
           (for-tuple ,vars in-relation ,(compile-expression rel))
             ,(compile-query-quals rest exprs)))))

(defun %compile-quantification (qual rest exprs compenv scope outermost)
  (let ((%add-qvar-compenv (flip #'add-qvar-compenv))
        (%scoped-symbol (alexandria:rcurry #'scoped-symbol scope)))
    (let ((vars (quantification-vars qual))
          (rel  (quantification-relation qual)))
      (let ((vars1 (mapcar %scoped-symbol vars))
            (compenv1 (reduce %add-qvar-compenv vars
                              :initial-value compenv)))
        (if outermost
            `(iterate:iter outermost
               (for-tuple ,vars1 in-relation
                          ,(%compile-expression rel compenv scope))
                 ,(%compile-query-quals rest exprs compenv1 scope))
            `(iterate:iter
               (for-tuple ,vars1 in-relation
                          ,(%compile-expression rel compenv scope))
                 ,(%compile-query-quals rest exprs compenv1 scope)))))))


;;;
;;; Compiler - Query - Predicate
;;;

(defun compile-predicate (pred rest exprs)
  `(when ,(compile-expression pred)
     ,(compile-query-quals rest exprs)))

(defun %compile-predicate (pred rest exprs compenv scope)
  `(when ,(%compile-expression pred compenv scope)
     ,(%compile-query-quals rest exprs compenv scope)))


;;;
;;; Compiler - Lisp form
;;;

(defun lisp-form-p (expr)
  (cl-pattern:match expr
    (('lisp _) t)
    (_ nil)))

(defun lisp-form (expr)
  (cl-pattern:match expr
    (('lisp form) form)
    (_ (error "invalid expression: ~S" expr))))

(defun compile-lisp-form (expr)
  (lisp-form expr))


;;;
;;; Compiler - Function application
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
  (cl-pattern:match expr
    ((op . _) (and (member op +generic-functions+)
                   t))
    (_ nil)))

(defun %function-p (expr)
  (and (consp expr)
       (car expr)
       t))

;; (defparameter +specialized-functions+
;;   (let ((alist (alexandria:plist-alist +function-table+)))
;;     (loop for (_ . candidates) in alist
;;        append (mapcar #'caddr candidates))))

;; (defun specialized-function-p (expr)
;;   (cl-pattern:match expr
;;     ((op . _) (and (member op +specialized-functions+)
;;                    t))
;;     (_ nil)))

(defun compile-function (expr)
  (cl-pattern:match expr
    (('user= x y) `(equalp ,(compile-expression x)
                           ,(compile-expression y)))
    (('event= x y) `(equalp ,(compile-expression x)
                            ,(compile-expression y)))
    ((op . args) `(,op ,@(mapcar #'compile-expression args)))
    (_ (error "invalid expression: ~S" expr))))

(defun %compile-function (expr compenv scope)
  (let ((operator (function-operator expr))
        (operands (function-operands expr)))
    (cond
      ((lookup-compenv operator compenv)
       (%compile-function-letfun operator operands compenv scope))
      (t
       (%compile-function-built-in operator operands compenv scope)))))

(defun %compile-function-letfun (operator operands compenv scope)
  (let ((%compile-expression (alexandria:rcurry #'%compile-expression
                                                compenv scope)))
    (cl-pattern:match (lookup-compenv operator compenv)
      ((:letfun args expr compenv1)
       ;; add args and operands compiled with compenv to compenv1 as
       ;; :argvar, then compile expr1 with new compenv1 and new scope
       (let ((compiled-operands (mapcar %compile-expression operands)))
         (let ((pairs (mapcar #'cons args compiled-operands)))
           (let ((compenv2
                  (reduce #'(lambda (%compenv pair)
                              (destructuring-bind (arg . operand) pair
                                (add-argvar-compenv arg operand %compenv)))
                          pairs
                          :initial-value compenv1)))
             (%compile-expression expr compenv2
                                  (scoping-symbol operator))))))
      (_ (error "symbol ~S is bound to variable" operator)))))

(defun %compile-function-built-in (operator operands compenv scope)
  (let ((%compile-expression (alexandria:rcurry #'%compile-expression
                                                compenv scope)))
    `(,operator ,@(mapcar %compile-expression operands))))


;;;
;;; Compiler - Scoping counter
;;;

(defvar *scoping-counter* 1)

(defun scoping-symbol (symbol)
  (prog1
      (symbolicate (list "%" symbol *scoping-counter*)
                   :package (symbol-package symbol))
    (incf *scoping-counter*)))

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

;; (defun inc-letvar-compenv (var compenv)
;;   (assert (symbolp var))
;;   (labels ((%inc-letvar-compenv (var elems)
;;              (cl-pattern:match elems
;;                (((var1 . :qvar) . rest)
;;                 (acons var1 :qvar (%inc-letvar-compenv var rest)))
;;                (((var1 . (:letvar cnt expr compenv1)) . rest)
;;                 (if (eq var1 var)
;;                     (acons var1 (list :letvar (1+ cnt) expr compenv1) rest)
;;                     (acons var1 (list :letvar cnt expr compenv1)
;;                            (%inc-letvar-compenv var rest))))
;;                (((var1 . (:letfun cnt args expr compenv1)) . rest)
;;                 (acons var1 (list :letfun cnt args expr compenv1)
;;                        (%inc-letvar-compenv var rest)))
;;                (_ (error "variable ~S does not exist" var)))))
;;     (with-%compenv-elements (elems compenv)
;;       (%inc-letvar-compenv var elems))))

(defun add-letfun-compenv (var args expr compenv)
  (assert (symbolp var))
  (assert (listp args))
  (with-%compenv-elements (elems compenv)
    (acons var (list :letfun args expr compenv)
           elems)))

;; (defun inc-letfun-compenv (var compenv)
;;   (assert (symbolp var))
;;   (labels ((%inc-letfun-compenv (var elems)
;;              (cl-pattern:match elems
;;                (((var1 . :qvar) . rest)
;;                 (acons var1 :qvar (%inc-letfun-compenv var rest)))
;;                (((var1 . (:letvar cnt expr compenv1)) . rest)
;;                 (acons var1 (list :letvar cnt expr compenv1)
;;                        (%inc-letfun-compenv var rest)))
;;                (((var1 . (:letfun cnt args expr compenv1)) . rest)
;;                 (if (eq var1 var)
;;                     (acons var1 (list :letfun (1+ cnt) args expr compenv1)
;;                            rest)
;;                     (acons var1 (list :letfun cnt expr compenv1) rest)))
;;                (_ (error "variable ~S does not exist" var)))))
;;     (with-%compenv-elements (elems compenv)
;;       (%inc-letfun-compenv var elems))))

(defun lookup-compenv (var compenv)
  (assert (symbolp var))
  (let ((elems (%compenv-elements compenv)))
    (cdr (assoc var elems))))

(defun print-compenv (compenv stream)
  (let ((elems (%compenv-elements compenv)))
    (format stream "#S~W" `(compenv ,@elems))))


;;;
;;; Utilities
;;;

(defun percent-symbol-p (symbol)
  (alexandria:starts-with #\% (princ-to-string symbol)))

(defun flip (function)
  (lambda (x y)
    (funcall function y x)))

(defun symbolicate (things &key package)
  (let* ((strs (mapcar #'princ-to-string things))
         (name (apply #'concatenate 'string strs)))
    (if package
        (intern name package)
        (intern name))))
