#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql)


;;;
;;; Relations
;;;

;;; User: < User >
(export '+u+)
(defrelation +u+ (:int)
  (relation-adjoin-all (list (tuple 1)
                             (tuple 2))
                       (empty-relation)))

;;; Event: < Event, User >
(export '+ev+)
(defrelation +ev+ (:int :int)
  (relation-adjoin-all (list (tuple 1 1)
                             (tuple 2 1)
                             (tuple 3 1)
                             (tuple 4 2)
                             (tuple 5 2))
                       (empty-relation)))

;;; Event Conversion: < Event, Conversion >
(export '+cv+)
(defrelation +cv+ (:int :int)
  (relation-adjoin-all (list (tuple 3 1)
                             (tuple 5 1))
                       (empty-relation)))

;;; Event Advertise: < Event, Advertise >
(export '+ad+)
(defrelation +ad+ (:int :int)
  (relation-adjoin-all (list (tuple 1 1)
                             (tuple 2 2)
                             (tuple 4 2))
                       (empty-relation)))

;;; Event Search: < Event, Search >
(export '+sr+)
(defrelation +sr+ (:int :int)
  (empty-relation))

;;; < Event, User, Advertise >
(export '+eua+)
(defrelation +eua+ (:int :int :int)
  (eval-waql (query (ev u ad) (<- (ev u) +ev+)
                              (<- (ev ad) +ad+))
             :sexp-p t))

;;; < Event, User, Conversion >
(export '+euc+)
(defrelation +euc+ (:int :int :int)
  (eval-waql (query (ev u cv) (<- (ev u) +ev+)
                              (<- (ev cv) +cv+))
             :sexp-p t))

;;; < User, Advertise Event, Conversion Event >
(defrelation +uf1+ (:int :int :int)
  (eval-waql (query (u ae ce) (<- (ae u ad) +eua+)
                              (<- (ce u cv) +euc+)
                              (< ae ce))
             :sexp-p t))


;;;
;;; test Querying
;;;

(defun test-querying ()

  (let ((result (eval-waql (query (u ae1 ae2 ce)
                                  (<- (u ae1 ce) +uf1+)
                                  (<- (u ae2 ce) +uf1+)
                                  (< ae1 ae2))
                           :sexp-p t)))
    (assert (relation-member (tuple 1 1 2 3) result))
    (assert (= (relation-count result) 1)))
  
  ;;; test for recursive query
  (let ((result (eval-waql
                  (query (u1 ad) (<- (ev u) +ev+)
                                 (<- (u1 ad) (query (u ad)
                                                    (<- (ev ad) +ad+))))
                  :sexp-p t)))
    (assert (relation-member (tuple 1 1) result))
    (assert (relation-member (tuple 2 2) result))
    (assert (= (relation-count result) 3)))
  
  ;;; conversion per advertises by times
  (print
    (eval-waql (query ( ad
                        (count (query (ae) (<- (_ ae _) +uf1+)
                                           (<- (ae ad) +ad+))))
                      (<- (_ ad) +ad+))
               :sexp-p t))
  
  ;;; conversion per advertises by UUs
  (print
    (eval-waql (query ( ad
                        (count (query (u) (<- (u ae _) +uf1+)
                                          (<- (ae ad) +ad+))))
                      (<- (_ ad) +ad+))
               :sexp-p t)))
