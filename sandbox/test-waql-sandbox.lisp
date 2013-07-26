#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-sandbox)

(plan nil)

;;;
;;; test User
;;;

(diag "test User")

;;; test USER constructor
(ok (user 1))

;;; test USER-ID accessor
(is (user-id (user 1)) 1)


;;;
;;; test Action Event
;;;

(diag "test Action Event")

;;; test ACTION-EVENT constructor
(ok (action-event 1))

;;; test ACTION-EVENT-ID accessor
(is (action-event-id (action-event 1)) 1)


;;;
;;; test Action
;;;

(diag "test Action")

;;; test ACTION constructor
(ok (action 1))

;;; test ACTION-ID accessor
(is (action-id (action 1)) 1)


;;;
;;; test Conversion Event
;;;

(diag "test Conversion Event")

;;; tset CONVERSION-EVENT constructor
(ok (conversion-event 1))

;;; test CONVERSION-EVENT-ID accessor
(is (conversion-event-id (conversion-event 1)) 1)


;;;
;;; test Conversion
;;;

(diag "test Conversion")

;;; test CONVERSION constructor
(ok (conversion 1))

;;; test CONVERSION-ID accessor
(is (conversion-id (conversion 1)) 1)


;;;
;;; test Querying
;;;

(diag "test Querying")

(defparameter +uf1+
  (relation-adjoin-all (list
                        (tuple (user 1) (action-event 1) (action 1)
                               (conversion-event 1) (conversion 1))
                        (tuple (user 1) (action-event 2) (action 2)
                               (conversion-event 1) (conversion 1))
                        (tuple (user 2) (action-event 3) (action 2)
                               (conversion-event 2) (conversion 2)))
                       (empty-relation)))

(let ((cl-test-more:*default-test-function* #'equalp)
      (result (query (u1 ae1 ac1 ce1 cv1 ae2 ac2 ce2 cv2)
                     (<- (u1 ae1 ac1 ce1 cv1) +uf1+)
                     (<- (u2 ae2 ac2 ce2 cv2) +uf1+)
                     (= (user-id u1) (user-id u2))
                     (< (action-event-id ae1) (action-event-id ae2)))))
  (ok (relation-member (tuple (user 1)
                              (action-event 1) (action 1)
                              (conversion-event 1) (conversion 1)
                              (action-event 2) (action 2)
                              (conversion-event 1) (conversion 1))
                       result))
  (is (relation-count result) 1))


(finalize)
