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
    (funcall (alexandria:rcurry #'solve-pattern-match (empty-patenv))
      (check-reserved-symbols expr))))


;;;
;;; Checking reserved symbols
;;;

(defun check-reserved-symbols (expr)
  (labels ((check (x)
             (unless (string/= "%" (subseq (princ-to-string x) 0 1))
               (error "symbol beginning with \"%\" is reserved: ~S" x))
             x))
    (mapcar-tree #'check expr)))


;;;
;;; Solving pattern match
;;;

(defun solve-pattern-match (expr patenv)
  (cond
    ((literal-p expr) expr)
    ((symbol-p expr) expr)
;;     ((tuple-p expr) expr)
    ((query-p expr) (solve-pattern-match-query expr patenv))
    ((lisp-form-p expr) (solve-pattern-match-lisp-form expr))
    ((function-p expr) (solve-pattern-match-function expr patenv))
    (t (error "invalid expression: ~S" expr))))

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

(defun solve-pattern-match-quantification (qual rest exprs patenv)
  (let ((vars (quantification-vars qual))
        (rel  (quantification-relation qual)))
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
            (solve-pattern-match-quals (append preds rest) exprs patenv1)
          (list qual1 rest1 exprs1)))))))

(defun solve-pattern-match-predicate (qual rest exprs patenv)
  (let ((qual1 (solve-pattern-match qual patenv)))
    (destructuring-bind (rest1 exprs1)
        (solve-pattern-match-quals rest exprs patenv)
      (list qual1 rest1 exprs1))))

(defun solve-pattern-match-lisp-form (expr)
  expr)

(defun solve-pattern-match-function (expr patenv)
  (cl-pattern:match expr
    (('= x y) `(= ,(solve-pattern-match x patenv)
                  ,(solve-pattern-match y patenv)))
    (('count x) `(count ,(solve-pattern-match x patenv)))
    (_ (error "invalid expression: ~S" expr))))


;;;
;;; Pattern matching environment
;;;

(defstruct (patenv (:constructor %make-patenv)
                   (:conc-name %patenv-)
                   (:print-object print-patenv))
  (variables nil :type list :read-only t)) ; alist { var -> counter }

(defun empty-patenv ()
  (%make-patenv))

(defmacro with-%patenv-variables ((vars patenv) &body form)
  `(let ((,vars (%patenv-variables ,patenv)))
     (%make-patenv :variables ,@form)))

(defun patenv-add (var patenv)
  (unless (null (patenv-lookup var patenv))
    (error "variable ~S already exists" var))
  (with-%patenv-variables (vars patenv)
    (acons var 1 vars)))

(defun patenv-inc (var patenv)
  (labels ((%patenv-inc (var vars)
             (cl-pattern:match vars
               (((var1 . cnt) . rest)
                (if (eq var1 var)
                    (acons var1 (1+ cnt) rest)
                    (acons var1 cnt (%patenv-inc var rest))))
               (_ (error "variable ~S does not exist" var)))))
    (with-%patenv-variables (vars patenv)
      (%patenv-inc var vars))))

(defun patenv-lookup (var patenv)
  (assoc var (%patenv-variables patenv)))

(defun print-patenv (patenv stream)
  (format stream "#S~W" `(patenv ,@(%patenv-variables patenv))))


;;;
;;; Pattern matcher
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

(defun pattern-matcher-match (var matcher)
  (with-%pattern-matcher ((vars patenv preds) matcher)
    (cl-pattern:match (patenv-lookup var patenv)
      ((_ . count)
       (let ((var1 (pattern-matcher-symbol var count)))
         (let ((vars1   (cons var1 vars))
               (patenv1 (patenv-inc var patenv))
               (preds1  (cons `(= ,var ,var1) preds)))
           (values vars1 patenv1 preds1))))
      (_
       (let ((vars1   (cons var vars))
             (patenv1 (patenv-add var patenv)))
         (values vars1 patenv1 preds))))))

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
;;; Compiler
;;;

(defun compile-expression (expr)
  (cond
    ((literal-p expr) (compile-literal expr))
    ((symbol-p expr) (compile-symbol expr))
;;     ((tuple-p expr) (compile-tuple expr))
    ((query-p expr) (compile-query expr))
    ((lisp-form-p expr) (compile-lisp-form expr))
    ((function-p expr) (compile-function expr))
    (t (error "invalid expression: ~S" expr))))


;;;
;;; Compiler - literal
;;;

(defun literal-p (expr)
  (typep expr 'fixnum))

(defun compile-literal (expr)
  (unless (literal-p expr)
    (error "invalid expression: ~S" expr))
  expr)


;;;
;;; Compiler - symbol
;;;

(defun symbol-p (expr)
  (symbolp expr))

(defun compile-symbol (expr)
  (unless (symbol-p expr)
    (error "invalid expression: ~S" expr))
  expr)


;;;
;;; Compiler - query
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

(defun compile-query-quals (quals exprs &key outermost)
  (assert (or (null outermost)
              (and outermost
                   (quantification-p (car quals)))))
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (compile-query-qual qual rest exprs outermost))
      (compile-query-exprs exprs)))

(defun compile-query-qual (qual rest exprs outermost)
  (cond
    ((quantification-p qual)
     (compile-quantification qual rest exprs outermost))
    (t (compile-predicate qual rest exprs))))

(defun compile-query-exprs (exprs)
  (let ((compiled-exprs (mapcar #'compile-expression exprs)))
    `(iterate:in outermost
       (collect-relation (tuple ,@compiled-exprs)))))


;;;
;;; Compiler - query - quantification
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
        `(iterate:iter (for-tuple ,vars in-relation
                                        ,(compile-expression rel))
           ,(compile-query-quals rest exprs)))))


;;;
;;; Compiler - query - predicate
;;;

(defun compile-predicate (pred rest exprs)
  `(when ,(compile-expression pred)
     ,(compile-query-quals rest exprs)))


;;;
;;; Compiler - lisp form
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
;;; Compiler - function
;;;

(defun function-p (expr)
  (cl-pattern:match expr
    (('= . _) t)
    (('count . _) t)
    (_ nil)))

(defun compile-function (expr)
  (cl-pattern:match expr
    (('= x y) `(equalp ,(compile-expression x)
                       ,(compile-expression y)))
    (('count x) `(relation-count ,(compile-expression x)))
    (_ (error "invalid expression: ~S" expr))))


;;;
;;; Utilities
;;;

(defun mapcar-tree (function tree)
  (labels ((rec (x)
             (cond ((null x) nil)
                   ((atom x) (funcall function x))
                   (t (cons (rec (car x))
                            (rec (cdr x)))))))
    (rec tree)))
