#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql.sandbox)


;;;
;;; Relations
;;;

;;; User: < User >
(defrelation +u+ (:int)
  (relation-adjoin-all (list (tuple 1)
                             (tuple 2))
                       (empty-relation)))

;;; Event: < Event, User >
(defrelation +ev+ (:int :int)
  (relation-adjoin-all (list (tuple 1 1)
                             (tuple 2 1)
                             (tuple 3 1)
                             (tuple 4 2)
                             (tuple 5 2))
                       (empty-relation)))

;;; Event Conversion: < Event, Conversion >
(defrelation +cv+ (:int :int)
  (relation-adjoin-all (list (tuple 3 1)
                             (tuple 5 1))
                       (empty-relation)))

;;; Event Advertise: < Event, Advertise >
(defrelation +ad+ (:int :int)
  (relation-adjoin-all (list (tuple 1 1)
                             (tuple 2 2)
                             (tuple 4 2))
                       (empty-relation)))

;;; Event Search: < Event, Search >
(defrelation +sr+ (:int :int)
  (empty-relation))

;;; < Event, User, Advertise >
(defrelation +eua+ (:int :int :int)
  (waql-in-sexp (query (ev u ad) (<- (ev u) +ev+)
                                 (<- (ev ad) +ad+))))

;;; < Event, User, Conversion >
(defrelation +euc+ (:int :int :int)
  (waql-in-sexp (query (ev u cv) (<- (ev u) +ev+)
                                 (<- (ev cv) +cv+))))

;;; < User, Advertise Event, Conversion Event >
(defrelation +uf1+ (:int :int :int)
  (waql-in-sexp (query (u ae ce) (<- (ae u ad) +eua+)
                                 (<- (ce u cv) +euc+)
                                 (< ae ce))))


;;;
;;; test Querying
;;;

(defun test-querying ()

  (let ((result (waql-in-sexp (query (u ae1 ae2 ce)
                                     (<- (u ae1 ce) +uf1+)
                                     (<- (u ae2 ce) +uf1+)
                                     (< ae1 ae2)))))
    (assert (relation-member result (tuple 1 1 2 3)))
    (assert (= (relation-count result) 1)))
  
  ;;; test for recursive query
  (let ((result (waql-in-sexp
                  (query (u1 ad) (<- (ev u) +ev+)
                                 (<- (u1 ad) (query (u ad)
                                                    (<- (ev ad) +ad+)))))))
    (assert (relation-member result (tuple 1 1)))
    (assert (relation-member result (tuple 2 2)))
    (assert (= (relation-count result) 3)))
  
  ;;; conversion per advertises by times
  (print
    (waql-in-sexp (query ( ad
                           (count (query (ae) (<- (_ ae _) +uf1+)
                                              (<- (ae ad) +ad+))))
                         (<- (_ ad) +ad+))))
  
  ;;; conversion per advertises by UUs
  (print
    (waql-in-sexp (query ( ad
                           (count (query (u) (<- (u ae _) +uf1+)
                                             (<- (ae ad) +ad+))))
                         (<- (_ ad) +ad+)))))
