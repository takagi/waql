#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage waql
  (:use :cl)
  (:export ;; User
           #:user
           ;; Event
           #:event
           #:event<
           ;; Tuple
           #:tuple
           #:with-tuple
           #:tuple-p
           ;; Relation
           #:relation
           #:empty-relation
           #:relation->list
           #:relation-member
           #:relation-count
           #:relation-adjoin
           #:relation-adjoin-all
           ;; Extending :ITERATE library for relation
           #:for-tuple
           #:in-relation
           #:collect-relation
           ;; Querying
           #:eval-waql
           #:query
           #:<-
           #:lisp
           #:defrelation
           ))
