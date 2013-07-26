#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql)

;;;
;;; User
;;;

(defstruct (user (:constructor user (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Action Event
;;;

(defstruct (action-event (:constructor action-event (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Action
;;;

(defstruct (action (:constructor action (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Conversion Event
;;;

(defstruct (conversion-event (:constructor conversion-event (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Conversion
;;;

(defstruct (conversion (:constructor conversion (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Tuple
;;;

(defstruct (tuple (:constructor %make-tuple)
                  (:conc-name %tuple-))
  (elements nil :type list :read-only t))

(defun tuple (&rest args)
  (%make-tuple :elements args))

(defmacro with-tuple (vars tuple &body body)
  `(destructuring-bind ,vars (%tuple-elements ,tuple)
     ,@body))


;;;
;;; Relation
;;;

(defstruct (relation (:constructor empty-relation ())
                     (:conc-name %relation-))
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
;;; Querying
;;;

(defun query-qual (qual)
  (cond
    ((quantification-p qual) (query-quantification qual))
    (t (error "invalid form: ~S" qual))))

(defun quantification-p (qual)
  (cl-pattern:match qual
    (('<- . _) t)
    (_ nil)))

(defun quantification-vars (qual)
  (cl-pattern:match qual
    (('<- vars _) (unless (listp vars)
                    (error "quantification variables must be list: ~S" vars))
                  vars)
    (_ (error "invalid form for quantification: ~S" qual))))

(defun quantification-relation (qual)
  (cl-pattern:match qual
    (('<- _ rel) rel)
    (_ (error "invalid form for quantification: ~S" qual))))

(defun query-quantification (qual)
  (let ((vars (quantification-vars qual))
        (rel  (quantification-relation qual)))
    `(for-tuple ,vars in-relation ,rel)))

(defun query-exps (exps)
  `(collect-relation (tuple ,@exps)))

(defmacro query (exps &rest quals)
  `(iterate:iter ,@(mapcar #'query-qual quals)
                 ,(query-exps exps)))
