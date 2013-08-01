#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-sandbox)


;;;
;;; Relations
;;;

(defrelation +ue1+ (:user :event)
  (relation-adjoin-all (list (tuple (user 1) (event 1))
                             (tuple (user 1) (event 2))
                             (tuple (user 1) (event 3))
                             (tuple (user 2) (event 4))
                             (tuple (user 2) (event 5)))
                       (empty-relation)))

(defrelation +ea1+ (:event :action)
  (relation-adjoin-all (list (tuple (event 1) (action 1))
                             (tuple (event 2) (action 2))
                             (tuple (event 4) (action 2)))
                       (empty-relation)))

(defrelation +ec1+ (:event :conversion)
  (relation-adjoin-all (list (tuple (event 3) (conversion 1))
                             (tuple (event 5) (conversion 1)))
                       (empty-relation)))

(defrelation +uae1+ (:user :event :action)
  (eval-waql (query (u e ac) (<- (u e) +ue1+)
                             (<- (e ac) +ea1+))))

(defrelation +uce1+ (:user :event :conversion)
  (eval-waql (query (u e cv) (<- (u e) +ue1+)
                             (<- (e cv) +ec1+))))

(defrelation +uf1+ (:user :event :event)
  (eval-waql (query (u ae ce) (<- (u ae ac) +uae1+)
                              (<- (u ce cv) +uce1+)
                              (< ae ce))))


;;;
;;; test Querying
;;;

(diag "test Querying")

(plan nil)

(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u ae1 ac1 ce1 cv1 ae2 ac2 ce2 cv2)
                                (<- (u ae1 ce1) +uf1+)
                                (<- (u ae2 ce2) +uf1+)
                                (<- (ae1 ac1) +ea1+)
                                (<- (ce1 cv1) +ec1+)
                                (<- (ae2 ac2) +ea1+)
                                (<- (ce2 cv2) +ec1+)
                                (< ae1 ae2)))))
  (ok (relation-member (tuple (user 1)
                              (event 1) (action 1) (event 3) (conversion 1)
                              (event 2) (action 2) (event 3) (conversion 1))
                       result))
  (is (relation-count result) 1))

;;; test for recursive query
(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql
                (query (u1 ac) (<- (u ev) +ue1+)
                               (<- (u1 ac) (query (u ac)
                                                  (<- (ev ac) +ea1+)))))))
  (ok (relation-member (tuple (user 1) (action 1))
                       result))
  (ok (relation-member (tuple (user 2) (action 2))
                       result))
  (is (relation-count result) 3))

;;; click counts per action
;; (eval-waql (query ( ac
;;                     (count (query (ae1) (<- (u1 ae1 ce1) +uf1+)
;;                                         (<- (ae1 ac) +ea+))))
;;                   (<- (_ ae _) +uf1+)
;;                   (<- (ae ac) +ea+)))


;;; clisk UUs per action


(finalize)
