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

;;; Event: < Event, User, Time >
(defrelation +ev+ (:int :int :time)
  (relation-adjoin-all
     (list (tuple 1 1 (parse-timestring "2013-4-1T00:00:00"))
           (tuple 2 1 (parse-timestring "2013-4-2T00:00:00"))
           (tuple 3 1 (parse-timestring "2013-4-3T00:00:00"))
           (tuple 4 2 (parse-timestring "2013-4-4T00:00:00"))
           (tuple 5 2 (parse-timestring "2013-4-5T00:00:00")))
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

;;; < Event, User, Advertise, TIme >
(defrelation +eua+ (:int :int :int :time)
  (waql-in-sexp (query (ev u ad tm) (<- (ev u tm) +ev+)
                                    (<- (ev ad) +ad+))))

;;; < Event, User, Conversion, Time >
(defrelation +euc+ (:int :int :int :time)
  (waql-in-sexp (query (ev u cv tm) (<- (ev u tm) +ev+)
                                    (<- (ev cv) +cv+))))

;;; < User, Advertise Event, Conversion Event >
(defrelation +uf1+ (:int :int :int)
  (waql-in-sexp (query (u ae ce) (<- (ae u _ at) +eua+)
                                 (<- (ce u _ ct) +euc+)
                                 (< at ct))))


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
                  (query (u1 ad) (<- (ev u tm) +ev+)
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
