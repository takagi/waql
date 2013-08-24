#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage waql
  (:use :cl :parser-combinators)
  (:import-from :anaphora
                :acond
                :aand
                :it)
  (:import-from :alexandria
                :with-gensyms
                :plist-alist
                :starts-with
                :starts-with-subseq
                :length=
                :hash-table-keys
                :appendf
                :curry
                :rcurry)
  (:export ;; User
           #:user
           ;; Event
           #:event
           #:event<
           ;; Action
           #:action
           ;; Conversion
           #:conversion
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
           #:using
           #:collect-relation
           ;; Language interface
           #:repl-waql
           ;; Querying
           #:eval-waql
           #:query
           #:<-
           #:_
           #:lisp
           #:defrelation
           ;; Web REPL
           #:start
           #:stop
           ))
