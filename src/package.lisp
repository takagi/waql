#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage waql
  (:use :cl)
  (:export ;; User
           #:user
           #:user-id
           #:user-p
           ;; Action Event
           #:action-event
           #:action-event-id
           #:action-event-p
           ;; Action
           #:action
           #:action-id
           #:action-p
           ;; Conversion Event
           #:conversion-event
           #:conversion-event-id
           #:conversion-event-p
           ;; Conversion
           #:conversion
           #:conversion-id
           #:conversion-p
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
           ;; Extending :ITERATE library for relation
           #:for-tuple
           #:in-relation
           #:collect-relation
           ;; Querying
           #:query))
