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
;;; Evaluating waql
;;;

(defmacro eval-waql (expr)
  (compile-expression
    (solve-pattern-match expr)))


;;;
;;; Solving pattern match
;;;

(defun solve-pattern-match (expr)
  (cond
;;     ((literal-p expr) expr)
;;     ((symbol-p expr) expr)
;;     ((tuple-p expr) expr)
    ((query-p expr) (solve-pattern-match-query expr))
    ((function-p expr) (solve-pattern-match-function expr))
;;     (t (error "invalid expression: ~S" expr))))
    (t expr)))

(defun solve-pattern-match-query (expr)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (solve-pattern-match-quals quals exprs)))

(defun solve-pattern-match-quals (quals exprs)
  (if quals
      (let ((qual (car quals))
            (rest (cdr quals)))
        (solve-pattern-match-qual qual rest exprs))
      (solve-pattern-match-exprs exprs)))

(defun solve-pattern-match-qual (qual rest exprs)
  (cond
    ((quantification-p qual)
     (solve-pattern-match-quantification qual rest exprs))
    (t (solve-pattern-match-predicate qual rest exprs))))

;; 戻り値はなに？
(defun solve-pattern-match-quantification (qual rest exprs)
  (let ((vars (quantification-vars qual))
        (rel  (quantification-relation qual)))
    (let ((qual1 ...)
          (rest1 (solve-pattern-match-quals rest))
          (exprs1 (...)))
      `(query ,exprs1 ,(cons qual1 rest1)))))


;;;
;;; Pattern matching environment
;;;

(defstruct (patenv (:constructor %make-patenv)
                   (:conc-name %patenv-))
  (variables nil :type list :read-only t)) ; alist { var -> counter }

(defun empty-patenv ()
  (%make-patenv))

(defun patenv-add (var patenv)
  (unless (null (patenv-lookup var patenv))
    (error "variable ~S already exists" var))
  (let ((vars (%patenv-variables patenv)))
    (%make-patenv :variables (acons var 1 vars))))

(defun patenv-inc (var patenv)
  ;; TODO: 書き直す
  (cl-pattern:match (%patenv-variables patenv)
    (((var1 . cnt) . rest) (if (eq var1 var)
                               (%make-patenv :variables
                                             (acons var1 (1+ cnt) rest))
                               (%make-patenv :variables
                                             (acons var1 cnt (%patenv-variables (patenv-inc var (%make-patenv :variables rest)))))))
    (_ (error "variable ~S does not exist" var))))

(defun patenv-lookup (var patenv)
  (assoc var (%patenv-variables patenv)))


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

(defun pattern-match (var matcher)
  ;; 書き直す。ワーカーラッパー変換
  (let ((vars   (%pattern-matcher-vars matcher))
        (patenv (%pattern-matcher-patenv matcher))
        (preds  (%pattern-matcher-preds matcher)))
    (anaphora:aif (patenv-lookup var patenv)
      (let ((var1 (symbolicate-with-count var (cdr anaphora:it))))
        (let ((vars1   (cons var1 vars))
              (patenv1 (patenv-inc var patenv))
              (preds1  (cons `(= ,var ,var1) preds)))
          (%make-pattern-matcher :vars vars1 :patenv patenv1 :preds preds1)))
      (let ((vars1  (cons var vars))
            (patenv1 (patenv-add var patenv))
            (preds1  preds))
        (%make-pattern-matcher :vars vars1 :patenv patenv1 :preds preds1)))))

(defun symbolicate-with-count (var cnt)
  (let ((strs (mapcar #'princ-to-string (list var cnt))))
    (intern (apply #'concatenate 'string strs)
            (symbol-package var))))



;;;
;;; Compiler
;;;

(defun compile-expression (expr)
  (cond
;;     ((literal-p expr) (compile-literal nil))
;;     ((symbol-p expr) (compile-symbol nil))
;;     ((tuple-p expr) (compile-tuple nil))
    ((query-p expr) (compile-query expr))
    ((function-p expr) (compile-function expr))
;;     (t (error "invalid expression: ~S" expr))))
    (t expr)))


;;;
;;; Compiler - query
;;;

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
        (alexandria:with-gensyms (var)
          `(iterate:iter outermost
                         (for-tuple ,var in-relation ,(compile-expression rel))
                         (destructuring-bind ,vars ,var
                           ,(compile-query-quals rest exprs))))
        (alexandria:with-gensyms (var)
          `(iterate:iter (for-tuple ,var in-relation ,(compile-expression rel))
                         (destructuring-bind ,vars ,var
                           ,(compile-query-quals rest exprs)))))))


;;;
;;; Compiler - query - predicate
;;;

(defun compile-predicate (pred rest exprs)
  `(when ,(compile-expression pred)
     ,(compile-query-quals rest exprs)))


;;;
;;; Compiler - function
;;;

(defun function-p (expr)
  (cl-pattern:match expr
    (('= . _) t)
    (_ nil)))

(defun compile-function (expr)
  (cl-pattern:match expr
    (('= x y) `(equalp ,x ,y))
    (_ (error "invalid expression: ~S" expr))))
