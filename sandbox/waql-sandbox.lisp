#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-sandbox)

;;;
;;; User
;;;

(defstruct (user (:constructor user (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Event
;;;

(defstruct (event (:constructor event (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Action
;;;

(defstruct (action (:constructor action (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Conversion
;;;

(defstruct (conversion (:constructor conversion (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Relations
;;;

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
  (eval-waql (query (u e ac) (<- (u e) +ue1+)
                             (<- (e ac) +ea1+))))

(defparameter +uce1+
  ;; < User, Event, Conversion >
  (eval-waql (query (u e cv) (<- (u e) +ue1+)
                             (<- (e cv) +ec1+))))

(defparameter +uf1+
  ;; < User, Action Event, Action, Conversion Event, Conversion >
  (eval-waql (query (u ae ac ce cv) (<- (u ae ac) +uae1+)
                                    (<- (u ce cv) +uce1+)
                                    (lisp (< (event-id ae) (event-id ce))))))


;;;
;;; test Querying
;;;

(diag "test Querying")

(plan nil)

(let ((cl-test-more:*default-test-function* #'equalp)
      (result (eval-waql (query (u ae1 ac1 ce1 cv1 ae2 ac2 ce2 cv2)
                                (<- (u ae1 ac1 ce1 cv1) +uf1+)
                                (<- (u ae2 ac2 ce2 cv2) +uf1+)
                                (lisp (< (event-id ae1) (event-id ae2)))))))
  (ok (relation-member (tuple (user 1)
                              (event 1) (action 1) (event 3) (conversion 1)
                              (event 2) (action 2) (event 3) (conversion 1))
                       result))
  (is (relation-count result) 1))

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


(finalize)
