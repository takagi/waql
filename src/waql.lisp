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

(defun tuple-dim (tuple)
  (let ((elements (%tuple-elements tuple)))
    (length elements)))

(defun tuple-ref (tuple index)
  (let ((elements (%tuple-elements tuple)))
    (unless (<= 0 index)
      (error "The value ~A is not zero nor positive integer." index))
    (unless (< index (length elements))
      (error "The value ~A is too large." index))
    (elt elements index)))


;;;
;;; Relation
;;;

(defstruct (relation-index (:constructor make-relation-index ())
                           (:conc-name %relation-index-))
  (body (make-hash-table :test #'equalp) :type hash-table :read-only t))

(defun add-relation-index (index key value)
  (symbol-macrolet ((body (%relation-index-body index))
                    (place (gethash key body)))
    (setf place (cons value place)))
  index)

(defun lookup-relation-index (index key)
  (symbol-macrolet ((body (%relation-index-body index)))
    (gethash key body)))


(defstruct (relation (:constructor empty-relation ())
                     (:conc-name %relation-)
                     (:print-object print-relation))
  (body (make-hash-table :test #'equalp) :type hash-table :read-only t)
  (indices nil))

(defun %relation-index (relation i)
  (symbol-macrolet ((indices (%relation-indices relation)))
    (elt indices i)))

(defun relation->list (relation)
  (hash-table-keys (%relation-body relation)))

(defun relation-member (tuple relation)
  (unless (tuple-p tuple)
    (error "The value ~A is not of type TUPLE." tuple))
  (symbol-macrolet ((body (%relation-body relation)))
    (gethash tuple body)))

(defun relation-count (relation)
  (symbol-macrolet ((body (%relation-body relation)))
    (hash-table-count body)))

(defun relation-exists (relation)
  (symbol-macrolet ((body (%relation-body relation)))
    (> (hash-table-count body) 0)))

(defun print-relation (relation stream)
  (format stream "#S~W" `(relation ,(relation-count relation)
                                   ,@(relation->list relation))))

(defun relation-adjoin (tuple relation)
  (unless (tuple-p tuple)
    (error "The value ~A is not of type TUPLE." tuple))
  (symbol-macrolet ((body (%relation-body relation))
                    (indices (%relation-indices relation)))
    (unless (gethash tuple body)
      ;; add item to body hash table
      (setf (gethash tuple body) t)
      ;; operations on per-attribute indices
      (let ((dim (tuple-dim tuple)))
        ;; if not, prepare indices
        (unless indices
          (setf indices (loop for i from 0 below dim
                           collect (make-relation-index))))
        ;; add tuple to each index
        (loop for i from 0 below dim
           do (let ((val (tuple-ref tuple i))
                    (index (elt indices i)))
                (add-relation-index index val tuple))))))
  relation)

(defun relation-adjoin-all (tuples relation)
  (reduce #'relation-adjoin tuples
          :initial-value relation
          :from-end t))

(defun i-val (key)
  (unless (and (listp key)
               (= 1 (length (remove nil key))))
    (error "The value ~S must be list with exactly one non-nil element." key))
  (let ((dim (length key)))
    (car (remove-if-not #'cadr
           (mapcar #'list (alexandria:iota dim) key)))))

(defun i-val-cnt (i-val relation)
  (destructuring-bind (i val) i-val
    (let ((index (%relation-index relation i)))
      (list i val (length (lookup-relation-index index val))))))

(defun minimize (list &key (key #'identity) (test #'<))
  (if list
    (destructuring-bind (top . rest) list
      (do ((nlist rest (cdr nlist))
           (result top))
          ((null nlist) (return result))
        (let ((lhs (funcall key (car nlist)))
              (rhs (funcall key result)))
          (if (funcall test lhs rhs) (setq result (car nlist))))))
    nil))

(defun relation-index-lookup (relation keys)
  (labels ((%i-val-cnt (i-val)
             (i-val-cnt i-val relation)))
    (cond
      ((= 0 (relation-count relation))
       (relation->list relation))
      ((null keys)
       (relation->list relation))
      (t
       (let ((i-vals (mapcar #'i-val keys)))
         (let ((i-val-cnts (mapcar #'%i-val-cnt i-vals)))
           (destructuring-bind (i val _) (minimize i-val-cnts :key #'caddr)
             (declare (ignorable _))
             (let ((index (%relation-index relation i)))
               (lookup-relation-index index val)))))))))


;;;
;;; Extending :ITERATE library for relation
;;;

;;; driver for relation

;; (defun %relation-elt-tuple-elements (relation index)
;;   (%tuple-elements (elt (relation->list relation) index)))

;; (defun %relation-length (relation)
;;   (length (relation->list relation)))

;; (iterate:defclause-sequence in-relation nil
;;   :access-fn '%relation-elt-tuple-elements
;;   :size-fn '%relation-length
;;   :sequence-type 'relation
;;   :element-type 'tuple
;;   :element-doc-string "Tuples of a relation")

;; (defmacro for-tuple (vars IN-RELATION relation)
;;   (unless (eq IN-RELATION 'in-relation)
;;     (error "Invalid FOR-TUPLE clause."))
;;   `(iterate:for ,vars in (mapcar #'%tuple-elements
;;                                  (relation->list ,relation))))

(defmacro for-tuple (vars IN-RELATION relation &optional USING keys)
  (unless (eq IN-RELATION 'in-relation)
    (error "Invalid FOR-TUPLE clause."))
  (when USING
    (unless (eq USING 'using)
      (error "Invalid FOR-TUPLE clause.")))
  `(iterate:for ,vars in (mapcar #'%tuple-elements
                                 (relation-index-lookup ,relation ,keys))))

;;; gatherer for relation

(defmacro collect-relation (expr)
  `(iterate:reducing ,expr
                  by #'(lambda (r i)
                         (relation-adjoin i r))
       initial-value (empty-relation)))


;;;
;;; Interval
;;;

(defun days (n)
  (unless (integerp n)
    (error "The value ~S is not integer." n))
  (list :interval n :day))

(defun interval-p (interval)
  (and (listp interval)
       (eq (car interval) :interval)))

(defun interval-amount (interval)
  (unless (interval-p interval)
    (error "The value ~S is invalid interval." interval))
  (cadr interval))

(defun interval-unit (interval)
  (unless (interval-p interval)
    (error "The value ~S is invalid interval." interval))
  (caddr interval))

(defun time+ (time interval)
  (let ((amount (interval-amount interval))
        (unit   (interval-unit interval)))
    (local-time:timestamp+ time amount unit)))

(defun time- (time interval)
  (let ((amount (interval-amount interval))
        (unit   (interval-unit interval)))
    (local-time:timestamp- time amount unit)))


;;;
;;; Evaluating WAQL
;;;

(defmacro eval-waql (expr &key sexp-p)
  (if (not sexp-p)
    (compile-waql (parse-waql expr))
    (compile-waql expr)))


;;;
;;; Compiling WAQL in S-expression
;;;

(defun compile-waql (expr)
  (compile-expression-top
    (specialize-function-top
       (solve-pattern-match-top expr))))


;;;
;;; Solving pattern match
;;;

(defun solve-pattern-match-top (expr)
  (solve-pattern-match expr (initial-patenv)))

(defun solve-pattern-match (expr patenv)
  (cond
    ((literal-p expr) expr)
    ((symbol-p expr) (solve-pattern-match-symbol expr patenv))
    ((let-p expr) (solve-pattern-match-let expr patenv))
    ((query-p expr) (solve-pattern-match-query expr patenv))
    ((lisp-form-p expr) expr)
    ((function-p expr) (solve-pattern-match-function expr patenv))
    (t (error "invalid expression: ~A" expr))))


;;;
;;; Solving pattern match - Symbol
;;;

(defun solve-pattern-match-symbol (expr patenv)
  (unless (null (percent-symbol-p expr))
    (error "symbol beginning with \"%\" is reserved: ~A" expr))
  (unless (lookup-patenv expr patenv)
    (error "The variable ~A is unbound." expr))
  expr)


;;;
;;; Solving pattern match - Let
;;;

(defun solve-pattern-match-let (expr patenv)
  (cond
    ((let-var-p expr) (solve-pattern-match-let-var expr patenv))
    ((let-fun-p expr) (solve-pattern-match-let-fun expr patenv))
    (t (error "invalid expression: ~A" expr))))

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
      (error "symbol beginning with \"%\" is reserved: ~A" vars))
    (unless (individual-variables-p vars)
      (error "duplicated variables: ~A" vars))
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
    (_ (error "invalid expression: ~A" expr))))


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
  (unless (not (member var '(t nil)))
    (error "The value ~S is not valid symbol." var))
  (unless (symbolp var)
    (error "The value ~S is not symbol." var))
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
  (unless (not (member var '(t nil)))
    (error "The value ~S is not valid symbol." var))
  (unless (symbolp var)
    (error "The value ~S is not valid symbol." var))
  (unless (integerp count)
    (error "The value ~S is not integer." count))
  (let ((strs (mapcar #'princ-to-string (list "%" var count))))
    (let ((symbol (intern (apply #'concatenate 'string strs)
                          (symbol-package var))))
      (setf (get symbol 'original-symbol) var)
      symbol)))

(defun original-symbol (symbol)
  (get symbol 'original-symbol))

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

(defun initial-patenv ()
  (bulk-add-patenv (predefined-relations-vars) (empty-patenv)))

(defun patenv (vars)
  (bulk-add-patenv vars (initial-patenv)))

(defmacro with-%patenv-elements ((elems patenv) &body form)
  `(let ((,elems (%patenv-elements ,patenv)))
     (%make-patenv :elements (progn ,@form))))

(defun bulk-add-patenv (vars patenv)
  (reduce (flip #'add-patenv) vars
          :initial-value patenv))

(defun add-patenv (var patenv)
  (assert (symbolp var))
  (unless (null (lookup-patenv var patenv))
    (error "variable ~A already exists" var))
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
               (_ (error "variable ~A does not exist" var)))))
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
    (t (error "invalid expression: ~A" expr))))


;;;
;;; Function specialization - Literal
;;;

(defun specialize-function-literal (expr)
  (cond
    ((int-literal-p expr) (list expr :int))
    ((string-literal-p expr) (list expr :string))
    ((time-literal-p expr) (list expr :time))
    (t (error "invalid expression: ~A" expr))))


;;;
;;; Function specialization - Symbol
;;;

(defun specialize-function-symbol (expr typenv)
  (unless (symbol-p expr)
    (error "invalid expression: ~A" expr))
  (acond
    ((lookup-typenv expr typenv)
     (unless (not (function-type-p it))
       (error "symbol ~A is bound to function" expr))
     (list expr it))
    ((lookup-predefined-relations expr)
     (list expr it))
    (t (error "unbound variable: ~A" expr))))


;;;
;;; Function specialization - Let
;;;

(defun specialize-function-let (expr typenv)
  (cond
    ((let-var-p expr) (specialize-function-let-var expr typenv))
    ((let-fun-p expr) (specialize-function-let-fun expr typenv))
    (t (error "invalid expression: ~A" expr))))

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
    (labels ((%lookup-typenv (var)
               (lookup-typenv var typenv)))
      (unless (notany #'%lookup-typenv vars)
        (error "variables ~A already exist in environment" vars)))
    (destructuring-bind (rel1 rel-type) (specialize-function rel typenv)
      ;; error if quantification binder is not of relation type
      (unless (relation-type-p rel-type)
        (error "quantification binder must be of relation type: ~A" rel))
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
      (error "predicate must have :bool reutrn type: ~A" pred))
    (destructuring-bind (rest1 exprs1 type1)
        (specialize-function-quals rest exprs typenv)
      (list pred1 rest1 exprs1 type1))))


;;;
;;; Function specialization - Lisp form
;;;

(defun specialize-function-lisp-form (expr)
  (unless (lisp-form-p expr)
    (error "invalid expression: ~A" expr))
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
      (t (error "The function ~A is undefined." operator)))))

(defun specialize-function-function-in-typenv (operator operands typenv)
  (labels ((%specialize-function (operand)
             (specialize-function operand typenv)))
    (let* ((pairs         (mapcar #'%specialize-function operands))
           (operands1     (mapcar #'car pairs))
           (operand-types (mapcar #'cadr pairs)))
      (let* ((type        (lookup-typenv operator typenv))
             (arg-types   (function-type-arg-types type))
             (return-type (function-type-return-type type)))
        (unless (function-type-p type)
          (error "symbol ~A is bound to variable" operator))
        (unless (length= arg-types operand-types)
          (error "invalid number of arguments: ~A" (length operands)))
        (unless (equal arg-types operand-types)
          (error "invalid type of arguments: ~A" `(,operator ,@operands)))
        (list (make-function operator operands1)
              return-type)))))

(defun specialize-function-generic-function (operator operands typenv)
  (labels ((%specialize-function (operand)
             (specialize-function operand typenv)))
      (let* ((pairs         (mapcar #'%specialize-function operands))
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
  '(=       (((:user :user)     :bool     user=)
             ((:event :event)   :bool     event=)
             ((:int :int)       :bool     =)
             ((:string :string) :bool     string=)
             ((:time :time)     :bool     local-time:timestamp=))
    +       (((:time :interval) :time     time+))
    -       (((:time :interval) :time     time-))
    <       (((:event :event)   :bool     event<)
             ((:int :int)       :bool     <)
             ((:time :time)     :bool     local-time:timestamp<))
    <=      (((:int :int)       :bool     <=)
             ((:time :time)     :bool     local-time:timestamp<=))
    >       (((:event :event)   :bool     event>)
             ((:int :int)       :bool     >)
             ((:time :time)     :bool     local-time:timestamp>))
    >=      (((:int :int)       :bool     >=)
             ((:time :time)     :bool     local-time:timestamp>=))
    count   (((:relation)       :int      relation-count))
    exists  (((:relation)       :bool     relation-exists))
    days    (((:int)            :interval days))
    user    (((:int)            :user     user))
    user-id (((:user)           :int      user-id))))

(defparameter +generic-functions+
  (let ((alist (plist-alist +function-table+)))
    (mapcar #'car alist)))

(defun lookup-generic-function (operator operand-types)
  (let ((candidates (getf +function-table+ operator)))
    (when candidates
      (let ((func (assoc operand-types candidates :test #'match-types-p)))
        (unless func
          (error "invalid argument types for function ~A : ~A"
                 operator operand-types))
        (cdr func)))))


;;;
;;; Function specialization - Type environment
;;;

(defun empty-typenv ()
  nil)

(defun %typenv-vars (typenv)
  ;; for predefined-relations' use
  (mapcar #'car typenv))

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
  (compile-expression expr (empty-compenv) nil nil))

(defun compile-expression (expr compenv scope lookup-keys)
  (cond
    ((literal-p expr) (compile-literal expr))
    ((symbol-p expr) (compile-symbol expr compenv scope lookup-keys))
    ((let-p expr) (compile-let expr compenv scope))
    ((query-p expr) (compile-query expr compenv scope lookup-keys))
    ((lisp-form-p expr) (compile-lisp-form expr))
    ((function-p expr) (compile-function expr compenv scope))
    (t (error "invalid expression: ~A" expr))))


;;;
;;; Compiler - Literal
;;;

(defun compile-literal (expr)
  (cond
    ((int-literal-p expr) expr)
    ((string-literal-p expr) expr)
    ((time-literal-p expr) (compile-literal-time expr))
    (t (error "invalid expression: ~A" expr))))

(defun compile-literal-time (expr)
  (let ((date (time-literal-date expr))
        (time (time-literal-time expr)))
    (let ((timestring (format nil "~AT~A" date time)))
      `(local-time:parse-timestring ,timestring))))


;;;
;;; Compiler - Symbol
;;;

(defun compile-symbol (expr compenv scope lookup-keys)
  (unless (symbol-p expr)
    (error "invalid expression: ~A" expr))
  (cond
    ((lookup-compenv expr compenv)
     (compile-symbol-in-compenv expr compenv scope lookup-keys))
    ((lookup-predefined-relations expr)
     (compile-symbol-in-predefined-relations expr))
    (t (error "unbound variable: ~A" expr))))


(defun compile-symbol-in-compenv (expr compenv scope lookup-keys)
  (cl-pattern:match (lookup-compenv expr compenv)
    (:qvar
     (scoped-symbol expr scope))
    ((:argvar expr1)
     expr1)
    ((:letvar expr1 compenv1)
     (let ((scope1 (scoping-symbol expr)))
       (compile-expression expr1 compenv1 scope1 lookup-keys)))
    ((:letfun . _)
     (error "symbol ~A is bound to function" expr))))

(defun compile-symbol-in-predefined-relations (expr)
  expr)


;;;
;;; Compiler - Let
;;;

(defun compile-let (expr compenv scope)
  (cond
    ((let-var-p expr) (compile-let-var expr compenv scope))
    ((let-fun-p expr) (compile-let-fun expr compenv scope))
    (t (error "invalid expression: ~A" expr))))

(defun compile-let-var (expr compenv scope)
  (let ((lvar  (let-var expr))
        (lexpr (let-expr expr))
        (lbody (let-body expr)))
    (let ((compenv1 (add-letvar-compenv lvar lexpr compenv)))
      (compile-expression lbody compenv1 scope nil))))

(defun compile-let-fun (expr compenv scope)
  (let ((lvar  (let-var expr))
        (largs (let-arg-vars expr))
        (lexpr (let-expr expr))
        (lbody (let-body expr)))
    (let ((compenv1 (add-letfun-compenv lvar largs lexpr compenv)))
      (compile-expression lbody compenv1 scope nil))))


;;;
;;; Compiler - Query
;;;

(defun compile-query (expr compenv scope lookup-keys)
  (let ((quals (query-quals expr))
        (exprs (query-exprs expr)))
    (compile-query-quals quals exprs compenv scope lookup-keys
                         :outermost t)))


(defun compile-query-quals (quals exprs compenv scope lookup-keys
                            &key outermost)
  (assert (or (null outermost)
              (and outermost
                   (quantification-p (car quals)))))
  (if quals
    (let ((qual (car quals))
          (rest (cdr quals)))
      (compile-query-qual qual rest exprs compenv scope lookup-keys
                          outermost))
    (compile-query-exprs exprs compenv scope)))

(defun compile-query-qual (qual rest exprs compenv scope lookup-keys
                           outermost)
  (cond
    ((quantification-p qual)
     (compile-quantification qual rest exprs compenv scope lookup-keys
                             outermost))
    (t (compile-predicate qual rest exprs compenv scope lookup-keys))))

(defun compile-query-exprs (exprs compenv scope)
  (labels ((%compile-expression (expr)
             (compile-expression expr compenv scope nil)))
    (let ((compiled-exprs (mapcar #'%compile-expression exprs)))
      `(iterate:in outermost
         (collect-relation (tuple ,@compiled-exprs))))))


;;;
;;; Compiler - Query - Quantification
;;;

(defun original-vars (vars)
  (labels ((underscore-notation->nil (var)
             (and (not (underscore-notation-p var))
                  var)))
    (mapcar #'underscore-notation->nil
      (mapcar #'original-symbol vars))))

(defun key-from-current-quantification (var vars compenv scope)
  (labels ((element (var0)
             (if (eq var var0) var nil)))
    (if var
      (let ((elements (mapcar #'element vars)))
        (make-lookup-key elements compenv scope))
      nil)))

(defun keys-from-current-quantification (vars compenv scope)
  (unless vars
    (error "The value ~S is invalid." vars))
  (unless (every #'symbol-p vars)
    (error "Some values in ~S are not valid symbol." vars))
  (let ((orig-vars (original-vars vars)))
    (labels ((%key-from-current-quantification (orig-var)
               (key-from-current-quantification orig-var orig-vars
                                                compenv scope)))
      (remove nil
        (mapcar #'%key-from-current-quantification orig-vars)))))

(defun key-from-ascent-quantification (lookup-key exprs vars)
  (let ((pair (symbol-expr-pair exprs lookup-key)))
    (if pair
      (let ((compenv (lookup-key-compenv lookup-key))
            (scope   (lookup-key-scope lookup-key)))
        (lookup-key-with-symbol-expr-pair pair vars compenv scope))
      nil)))

(defun keys-from-ascent-quantification (keys exprs vars)
  (labels ((%key-from-ascent-quantification (key)
             (key-from-ascent-quantification key exprs vars)))
    (remove nil
      (mapcar #'%key-from-ascent-quantification keys))))

(defun compile-quantification (qual rest exprs compenv scope lookup-keys
                               outermost)
  (labels ((%scoped-symbol (symbol)
             (scoped-symbol symbol scope)))
    (let ((vars (quantification-vars qual))
          (rel  (quantification-relation qual)))
      (let ((vars1 (mapcar #'%scoped-symbol vars))
            (compenv1 (reduce (flip #'add-qvar-compenv) vars
                              :initial-value compenv))
            (lookup-keys1
              (append
                (keys-from-ascent-quantification lookup-keys exprs vars)
                (keys-from-current-quantification vars compenv scope))))
        `(iterate:iter ,@(if outermost '(outermost))
           (for-tuple ,vars1
             in-relation ,(compile-expression rel compenv scope lookup-keys1)
             ,@(if lookup-keys1 `(using ,(compile-lookup-keys lookup-keys1))))
           ,(compile-query-quals rest exprs compenv1 scope lookup-keys))))))


;;;
;;; Compiler - Query - Quantification - Symbol-Expr pair
;;;

(defun symbol-expr-pair (exprs lookup-key)
  (unless (listp exprs)
    (error "The value ~S is not list." exprs))
  (unless (lookup-key-p lookup-key)
    (error "The value ~S is not lookup key." lookup-key))
  (unless (= (length exprs) (lookup-key-dimension lookup-key))
    (error "The dimensions of ~S and ~S are inconsistent." exprs lookup-key))
  (labels ((aux (expr elem)
             (if (and elem (symbol-p expr))
               (cons expr elem)
               nil)))
    (find-if #'identity
      (mapcar #'aux exprs (lookup-key-elements lookup-key)))))

(defun lookup-key-with-symbol-expr-pair (symbol-expr-pair vars compenv scope)
  (unless (listp symbol-expr-pair)
    (error "The value ~S is not list." symbol-expr-pair))
  (unless (listp vars)
    (error "The value ~S is not list." vars))
  (unless (every #'symbol-p vars)
    (error "Some values in ~S are not valid symbol." vars))
  (if symbol-expr-pair
    (destructuring-bind (symbol . expr) symbol-expr-pair
      (labels ((aux (var)
                 (if (and var (eq var symbol))
                   expr
                   nil)))
        (let ((elements (mapcar #'aux vars)))
          (if (null (every #'null elements))
            (make-lookup-key elements compenv scope)
            nil))))))


;;;
;;; Compiler - Query - Quantification - Lookup key
;;;

(defun lookup-key-elements-p (elements)
  (and (listp elements)
       (single (remove nil elements))
       (symbol-p (find-if #'identity elements))))

(defun make-lookup-key (elements compenv scope)
  (unless (lookup-key-elements-p elements)
    (error "The value ~S is invalid for lookup key elements." elements))
  (check-type compenv compenv)
  (when scope
    (unless (symbol-p scope)
      (error "The value ~S is invalid for scoping symbol." scope)))
  (list :lookup-key elements compenv scope))

(defun lookup-key-p (lookup-key)
  (and (consp lookup-key)
       (eq (car lookup-key) :lookup-key)))

(defun lookup-key-elements (lookup-key)
  (unless (lookup-key-p lookup-key)
    (error "The value ~S is not lookup key." lookup-key))
  (cadr lookup-key))

(defun lookup-key-dimension (lookup-key)
  (length (lookup-key-elements lookup-key)))

(defun lookup-key-compenv (lookup-key)
  (unless (lookup-key-p lookup-key)
    (error "The value ~S is not lookup key." lookup-key))
  (caddr lookup-key))

(defun lookup-key-scope (lookup-key)
  (unless (lookup-key-p lookup-key)
    (error "The value ~S is not lookup key." lookup-key))
  (cadddr lookup-key))

(defun compile-lookup-key (lookup-key)
  (let ((elements (lookup-key-elements lookup-key))
        (compenv  (lookup-key-compenv lookup-key))
        (scope    (lookup-key-scope lookup-key)))
    (labels ((%compile-expression (element)
               (if element
                 (compile-expression element compenv scope nil)
                 nil)))
      `(list ,@(mapcar #'%compile-expression elements)))))

(defun compile-lookup-keys (keys)
  `(list ,@(mapcar #'compile-lookup-key keys)))


;;;
;;; Compiler - Query - Predicate
;;;

(defun compile-predicate (pred rest exprs compenv scope lookup-keys)
  `(when ,(compile-expression pred compenv scope nil)
     ,(compile-query-quals rest exprs compenv scope lookup-keys)))


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
  (labels ((%compile-expression (operand)
             (compile-expression operand compenv scope nil)))
    (cl-pattern:match (lookup-compenv operator compenv)
      ((:letfun args expr compenv1)
       ;; add args and operands compiled with compenv to compenv1 as
       ;; :argvar, then compile expr1 with new compenv1 and new scope
       (let ((compiled-operands (mapcar #'%compile-expression operands)))
         (unless (length= args compiled-operands)
           (error "invalid number of arguments: ~A"
                  (length compiled-operands)))
         (let ((pairs (mapcar #'cons args compiled-operands)))
           (let ((compenv2
                  (reduce #'(lambda (%compenv pair)
                              (destructuring-bind (arg . operand) pair
                                (add-argvar-compenv arg operand %compenv)))
                          pairs
                          :initial-value compenv1)))
             (compile-expression expr compenv2
                                 (scoping-symbol operator) nil)))))
      (_ (error "symbol ~A is bound to variable" operator)))))

(defun compile-function-built-in (operator operands compenv scope)
  (labels ((%compile-expression (operand)
             (compile-expression operand compenv scope nil)))
    `(,operator ,@(mapcar #'%compile-expression operands))))


;;;
;;; Compiler - Scoping
;;;

(defvar *scoping-count* 1)

(defun scoping-symbol (symbol)
  (prog1
      (format-symbol (symbol-package symbol) "%~A~A" symbol *scoping-count*)
    (incf *scoping-count*)))

(defun scoped-symbol (symbol scope)
  (if scope
      (let ((symbol-package (symbol-package symbol))
            (scope-package (symbol-package scope)))
        (unless (eq symbol-package scope-package)
          (error "The value ~S and ~S are not in same package." symbol scope))
        (format-symbol symbol-package "~A.~A" scope symbol))
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
  (or (int-literal-p expr)
      (string-literal-p expr)
      (time-literal-p expr)))

(defun int-literal-p (expr)
  (typep expr 'fixnum))

(defun string-literal-p (expr)
  (typep expr 'string))

(defun time-literal-p (expr)
  (cl-pattern:match expr
    (('time . _) t)
    (_ nil)))

(defun time-literal-date (expr)
  (cl-pattern:match expr
    (('time date _) (unless (stringp date)
                      (error "invalid expression: ~A" expr))
                    date)
    (_ (error "invalid expression: ~A" expr))))

(defun time-literal-time (expr)
  (cl-pattern:match expr
    (('time _ time) (unless (stringp time)
                      (error "invalid expression: ~A" expr))
                    time)
    (_ (error "invalid expression: ~A" expr))))


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
    (_ (error "invalid expression: ~A" expr))))

(defun let-args (expr)
  (cl-pattern:match expr
    (('let (_ args _) _) args)
    (_ (error "invalid expression: ~A" expr))))

(defun let-arg-vars (expr)
  (mapcar #'car (let-args expr)))

(defun let-arg-types (expr)
  (mapcar #'cadr (let-args expr)))

(defun let-expr (expr)
  ;; use optima instead of cl-pattern because of uncapability in this case
  (optima:match expr
    ((list 'let (list _ expr1) _) expr1)
    ((list 'let (list _ _ expr1) _) expr1)
    (_ (error "invalid expression: ~A" expr))))

(defun let-body (expr)
  ;; use optima instead of cl-pattern because of uncapability in this case
  (optima:match expr
    ((list 'let (list _ _) body) body)
    ((list 'let (list _ _ _) body) body)
    (_ (error "invalid expression: ~A" expr))))


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
    (_ (error "invalid expression: ~A" expr))))

(defun query-quals (expr)
  (cl-pattern:match expr
    (('query _ . quals) quals)
    (_ (error "invalid expression: ~A" expr))))


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
       (error "invalid expression: ~A" qual))
     vars)
    (_ (error "invalid expression: ~A" qual))))

(defun quantification-relation (qual)
  (cl-pattern:match qual
    (('<- _ rel) rel)
    (_ (error "invalid expression: ~A" qual))))


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
    (_ (error "invalid expression: ~A" expr))))


;;;
;;; Syntax - Function
;;;

(defun make-function (operator operands)
  `(,operator ,@operands))

(defun function-operator (expr)
  (cl-pattern:match expr
    ((operator . _) operator)
    (_ (error "invalid expression: ~A" expr))))

(defun function-operands (expr)
  (cl-pattern:match expr
    ((_ . operands) operands)
    (_ (error "invalid expression: ~A" expr))))

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
     (error "invalid relation type pattern: ~A" pattern))
    ((:relation '_ . attrs)
     (or (every #'wildcard-p attrs)
         (error "invalid relation type pattern: ~A" pattern)))
    ((:relation . attrs)
     (or (notany #'wildcard-p attrs)
         (error "invalid relation type pattern: ~A" pattern)))
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
    (error "pattern ~A is not relation type pattern" pattern))
  (cl-pattern:match pattern
    (:relation (error "relation type pattern of general does not have explicit attributes: ~A" pattern))
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
  (and (member type '(:bool :int :string :time :interval
                      :user :event :action :conversion))
       t))


;;;
;;; Type - Relation type
;;;

(defun make-relation-type (types)
  (dolist (type types)
    (unless (scalar-type-p type)
      (error "The value ~S is invalid type." type)))
  `(:relation ,@types))

(defun relation-type-p (type)
  (cl-pattern:match type
    ((:relation _ . _) t)
    (_ nil)))

(defun relation-type-attrs (type)
  (unless (relation-type-p type)
    (error "invalid relation type: ~A" type))
  (cdr type))

(defun relation-type-dim (type)
  (length (relation-type-attrs type)))


;;;
;;; Type - Functon type
;;;

(defun make-function-type (arg-types return-type)
  (unless (every #'scalar-type-p arg-types)
    (error "invalid types: ~A" arg-types))
  (unless (scalar-type-p return-type)
    (error "invalid type: ~A" return-type))
  `(:function ,arg-types ,return-type))

(defun function-type-p (type)
  (cl-pattern:match type
    ((:function _ _) t)
    (_ nil)))

(defun function-type-arg-types (type)
  (unless (function-type-p type)
    (error "invalid function type: ~A" type))
  (cadr type))

(defun function-type-return-type (type)
  (unless (function-type-p type)
    (error "invalid function type: ~A" type))
  (caddr type))


;;;
;;; Predefined relations
;;;

(defun make-predefined-relations ()
  (empty-typenv))

(defvar *predefined-relations* (make-predefined-relations))

(defun predefined-relations-vars (&optional (predefined-relations
                                             *predefined-relations*))
  (%typenv-vars predefined-relations))

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
;;; Parsing WAQL
;;;

(define-condition waql-parse-error (error)
  ((string :reader waql-parse-error-string
           :initarg :string
           :type string))
  (:report waql-parse-error-printer))

(defun waql-parse-error-printer (condition stream)
  (let ((string (waql-parse-error-string condition)))
    (format stream "Parse error: ~A" string)))

(defun parse-waql (string &key (top-expr-p t))
  (let ((parser (if top-expr-p
                  (expr-top*)
                  (expr*))))
    (multiple-value-bind (result suffix success front)
        (parse-string* parser string :complete t)
      (declare (ignorable suffix front))
      (unless success
        (let ((front-string (front-string front string)))
          (error 'waql-parse-error :string front-string)))
      result)))

(defparameter +front-string-margin+ 0)

(defun front-string (front string)
  (let ((pos (position-of front)))
    (if (<= pos +front-string-margin+)
      string
      (concatenate 'string "..."
        (subseq string (- pos +front-string-margin+))))))

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

(defun any-but-whitespace? ()
  (except? (any?) (whitespace?)))

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
  (~ws? (choices (int?)
                 (quoted?)
                 (time?))))

(defun literal* ()
  (~ws* (choices1 (int*)
                  (quoted?)
                  (time*))))

(defun time-date? ()
  (~ws? (between? (any-but-whitespace?) 1 nil 'string)))

(defun time-time? ()
  (~ws? (between? (any-but-whitespace?) 1 nil 'string)))

(defun time? ()
  (named-seq? (~ws? "time")
              (parser-combinators:<- date (time-date?))
              (parser-combinators:<- time (time-time?))
              (list 'time date time)))

(defun time-date* ()
  (~ws* (between? (any-but-whitespace?) 1 nil 'string)))

(defun time-time* ()
  (~ws* (between? (any-but-whitespace?) 1 nil 'string)))

(defun time* ()
  (named-seq* (~ws* "time")
              (parser-combinators:<- date (time-date*))
              (parser-combinators:<- time (time-time*))
              (list 'time date time)))

(defun reserved? ()
  (choices "let" "in" "time" "interval" "bool" "int" "string"))

(defun reserved* ()
  (choices1 "let" "in" "time" "interval" "bool" "int" "string"))

(defun symbol? ()
  (choice (plus-enclosed-symbol?)
          (ordinal-symbol?)))

(defun plus-enclosed-symbol? ()
  (~ws? (named-seq? #\+
                    (parser-combinators:<- symbol (symbol-string?))
                    #\+
                    (alexandria:symbolicate
                      (string-upcase
                        (concatenate 'string
                          "+" symbol "+"))))))

(defun ordinal-symbol? ()
  (~ws? (except? (named-seq? (parser-combinators:<- symbol (symbol-string?))
                             (alexandria:symbolicate
                               (string-upcase symbol)))
                 (reserved?))))

(defun symbol* ()
  (choice1 (plus-enclosed-symbol*)
           (ordinal-symbol*)))

(defun plus-enclosed-symbol* ()
  (~ws* (named-seq* #\+
                    (parser-combinators:<- symbol (symbol-string*))
                    #\+
                    (alexandria:symbolicate
                      (string-upcase
                        (concatenate 'string
                          "+" symbol "+"))))))

(defun ordinal-symbol* ()
  (~ws* (except? (named-seq* (parser-combinators:<- symbol (symbol-string*))
                             (alexandria:symbolicate
                               (string-upcase symbol)))
                 (reserved*))))

(defun symbol-string? ()
  (named-seq? (parser-combinators:<- head (symbol-head?))
              (parser-combinators:<- tail (symbol-tail?))
              (concatenate 'string
                (cons head tail))))

(defun symbol-string* ()
  (named-seq* (parser-combinators:<- head (symbol-head*))
              (parser-combinators:<- tail (symbol-tail*))
              (concatenate 'string
                (cons head tail))))

(defun symbol-head? ()
  (letter?))

(defun symbol-head* ()
  (letter?))

(defun symbol-tail? ()
  (many? (choice (alphanum?) #\-)))

(defun symbol-tail* ()
  (many* (choice1 (alphanum?) #\-)))

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
               `((,(additive-op?)   :left)
                 (,(comparison-op?) :left))))

(defun infix-fexpr* ()
  (expression* (infix-aexpr*)
               `((,(additive-op*)   :left)
                 (,(comparison-op*) :left))))

(def-cached-parser additive-op?
  (choice (mdo (~ws? #\+) (result (curry #'list '+)))
          (mdo (~ws? #\-) (result (curry #'list '-)))))

(def-cached-parser additive-op*
  (choice1 (mdo (~ws* #\+) (result (curry #'list '+)))
           (mdo (~ws* #\-) (result (curry #'list '-)))))

(def-cached-parser comparison-op?
  (choices (mdo (~ws? ">=") (result (curry #'list '>=)))
           (mdo (~ws? #\>)  (result (curry #'list '>)))
           (mdo (~ws? "<=") (result (curry #'list '<=)))
           (mdo (~ws? #\<)  (result (curry #'list '<)))
           (mdo (~ws? #\=)  (result (curry #'list '=)))))

(def-cached-parser comparison-op*
  (choices1 (mdo (~ws* ">=") (result (curry #'list '>=)))
            (mdo (~ws* #\>)  (result (curry #'list '>)))
            (mdo (~ws* "<=") (result (curry #'list '<=)))
            (mdo (~ws* #\<)  (result (curry #'list '<)))
            (mdo (~ws* #\=)  (result (curry #'list '=)))))

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
           (ty-string?)
           (ty-time?)
           (ty-interval?)
           (ty-relation?)))

(defun type* ()
  (choices1 (ty-bool*)
            (ty-int*)
            (ty-string*)
            (ty-time*)
            (ty-interval*)
            (ty-relation*)))

(defun ty-bool? ()
  (named-seq? (~ws? "bool") :bool))

(defun ty-bool* ()
  (named-seq* (~ws* "bool") :bool))

(defun ty-int? ()
  (named-seq? (~ws? "int") :int))

(defun ty-int* ()
  (named-seq* (~ws* "int") :int))

(defun ty-string? ()
  (named-seq? (~ws? "string") :string))

(defun ty-string* ()
  (named-seq* (~ws* "string") :string))

(defun ty-time? ()
  (named-seq? (~ws? "time") :time))

(defun ty-time* ()
  (named-seq* (~ws* "time") :time))

(defun ty-interval? ()
  (named-seq? (~ws? "interval") :interval))

(defun ty-interval* ()
  (named-seq* (~ws* "interval") :interval))

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

(defun single (list)
  (and (consp list)
       (null (cdr list))))

(defun left-trim (string)
  (string-left-trim '(#\Space #\Tab #\Newline) string))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))
