#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-sandbox)

(plan nil)


;;;
;;; test Querying
;;;

(diag "test Querying")

(defparameter +ue1+
  ;; < User, Event >
  (relation-adjoin-all (list
                        (tuple (user 1) (event 1))
                        (tuple (user 1) (event 2))
                        (tuple (user 1) (event 3))
                        (tuple (user 2) (event 4))
                        (tuple (user 2) (event 5)))
                       (empty-relation)))

(defparameter +ea1+
  ;; < Event, Action >
  (relation-adjoin-all (list
                        (tuple (event 1) (action 1))
                        (tuple (event 2) (action 2))
                        (tuple (event 4) (action 2)))
                       (empty-relation)))

(defparameter +ec1+
  ;; < Event, Conversion >
  (relation-adjoin-all (list
                        (tuple (event 3) (conversion 1))
                        (tuple (event 5) (conversion 1)))
                       (empty-relation)))

(defparameter +uae1+
  ;; < User, Event, Action >
  ;; { < u, e, ac > | < u, e >  ∈ UE1
  ;;                , < e, ac > ∈ EA1 }
  (eval-waql (query (u e ac) (<- (u e) +ue1+)
                             (<- (e ac) +ea1+))))

(defparameter +uce1+
  ;; < User, Event, Conversion >
  ;; { < u, e, cv > | < u, e >  ∈ UE1
  ;;                , < e, cv > ∈ EC1 }
  (eval-waql (query (u e cv) (<- (u e) +ue1+)
                             (<- (e cv) +ec1+))))

(defparameter +uf1+
  ;; < User, Action Event, Action, Conversion Event, Conversion >
  (eval-waql (query (u ae ac ce cv) (<- (u ae ac) +uae1+)
                                    (<- (u ce cv) +uce1+)
                                    (< (event-id ae) (event-id ce)))))

(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u ae1 ac1 ce1 cv1 ae2 ac2 ce2 cv2)
                                (<- (u ae1 ac1 ce1 cv1) +uf1+)
                                (<- (u ae2 ac2 ce2 cv2) +uf1+)
                                (< (event-id ae1) (event-id ae2))
                                ))))
  (ok (relation-member (tuple (user 1)
                              (event 1) (action 1) (event 3) (conversion 1)
                              (event 2) (action 2) (event 3) (conversion 1))
                       result))
  (is (relation-count result) 1))


(finalize)
