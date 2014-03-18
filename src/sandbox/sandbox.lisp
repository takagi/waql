#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql.sandbox)


;;;
;;; WAQL codes
;;;

(defparameter +business-goal-1-id+ 0)

(defparameter +business-goal-1-query+
  "-- �]�E�T�C�g �r�W�l�X�S�[���P�F�Z������l��
   -- ����ڐG�L�����ƂɈȉ����W�v
   -- �E�w����ԓ��ɐV�K�o�^�������[�U�̐�
   -- �E�V�K�o�^����30���ȓ��ɉ��債�����[�U�̐�
   -- �E�V�K�o�^����31���ȏ�90���ȓ��ɉ��債�����[�U�̐��i�⏕�w�W�j
   let bg := 0 in  -- �Z������l���r�W�l�X�S�[��
   let from := time ~A 00:00:00 in
   let to   := time ~A 00:00:00 in
   let oubo    := 0 in  -- ����R���o�[�W����
   let touroku := 1 in  -- �o�^�R���o�[�W����
   let is_first_contact u:int ae:int
               := -- ���[�Uu�ɂ��āA�L���ڐGae������ڐG���ǂ���
                  not (exists { <ae1> | <ae , u, _, at1> <- +eua+
                                      , <ae1, u, _, at2> <- +eua+
                                      , at2 < at1 })
   in { < cp_name, md_name
        , -- �w����ԓ��ɐV�K�o�^�������[�U�̐�
          count { <u> | -- �L��ad�ڐG�C�x���g
                        <ae, u, ad, _> <- +eua+
                      , -- ����ڐG���L��ad�ł���
                        is_first_contact u ae
                      , -- �V�K�o�^�R���o�[�W�����C�x���g
                        <_, u, touroku, ct> <- +euc+
                      , -- �V�K�o�^�R���o�[�W�������w����ԓ�
                        from <= ct, ct < to }
        , -- �V�K�o�^����30���ȓ��ɉ��債�����[�U�̐�
          count { <u> | -- �L��ad�ڐG�C�x���g
                        <ae, u, ad, _> <- +eua+
                      , -- ����ڐG���L��ad�ł���
                        is_first_contact u ae
                      , -- �V�K�o�^�R���o�[�W�����C�x���g
                        <_, u, touroku, ct1> <- +euc+
                      , -- �V�K�o�^�R���o�[�W�������w����ԓ�
                        from <= ct1, ct1 < to
                      , -- ����R���o�[�W�����C�x���g
                        <_, u, oubo, ct2> <- +euc+
                      , -- �V�K�o�^����30���ȓ��ɉ���
                        ct1 < ct2, ct2 <= ct1 + days 30 }
        , -- �V�K�o�^����31���ȏ�90���ȓ��ɉ��債�����[�U�̐�
          count { <u> | -- �L��ad�ڐG�C�x���g
                        <ae, u, ad, _> <- +eua+
                      , -- ����ڐG���L��ad�ł���
                        is_first_contact u ae
                      , -- �V�K�o�^�R���o�[�W�����C�x���g
                        <_, u, touroku, ct1> <- +euc+
                      , -- �V�K�o�^�R���o�[�W�������w����ԓ�
                        from <= ct1, ct1 < to
                      , -- ����R���o�[�W�����C�x���g
                        <_, u, oubo, ct2> <- +euc+
                      , -- �V�K�o�^����31���ȏ�90���ȓ��ɉ���
                        ct1 + days 30 < ct2, ct2 <= ct1 + days 90
                      , -- �V�K�o�^����30���ȓ��ɂ͉��債�Ă��Ȃ�
                        not (exists { <u> | -- ����R���o�[�W�����C�x���g
                                            <_, u, oubo, ct3> <- +euc+
                                          , -- �V�K�o�^����30���ȓ��ɂ͉��債�Ă��Ȃ�
                                            ct1 < ct3, ct3 <= ct1 + days 30
                                          } ) }
        >
      | <ad,bg,cp,md,_,_,_> <- +ad+
      , <cp,cp_name> <- +cp+
      , <md,md_name> <- +md+ }")

(defparameter +business-goal-2-id+ 1)

(defparameter +business-goal-2-query+
  "-- �]�E�T�C�g �r�W�l�X�S�[���Q�F��������l��
   -- ����ڐG�L�����ƂɈȉ����W�v
   -- �E�w����ԓ��ɐV�K�o�^�������[�U�̐�
   -- �E�V�K�o�^����31���ȏ�90���ȓ��ɉ��債�����[�U�̐�
   -- �E�V�K�o�^����30���ȓ��ɉ��債�����[�U�̐��i�⏕�w�W�j
   let bg := 1 in  -- ��������l���r�W�l�X�S�[��
   let from := time ~A 00:00:00 in
   let to   := time ~A 00:00:00 in
   let oubo    := 0 in  -- ����R���o�[�W����
   let touroku := 1 in  -- �o�^�R���o�[�W����
   let is_first_contact u:int ae:int
               := -- ���[�Uu�ɂ��āA�L���ڐGae������ڐG���ǂ���
                  not (exists { <ae1> | <ae , u, _, at1> <- +eua+
                                      , <ae1, u, _, at2> <- +eua+
                                      , at2 < at1 })
   in { < cp_name, md_name
        , -- �w����ԓ��ɐV�K�o�^�������[�U�̐�
          count { <u> | -- �L��ad�ڐG�C�x���g
                        <ae, u, ad, _> <- +eua+
                      , -- ����ڐG���L��ad�ł���
                        is_first_contact u ae
                      , -- �V�K�o�^�R���o�[�W�����C�x���g
                        <_, u, touroku, ct> <- +euc+
                      , -- �V�K�o�^�R���o�[�W�������w����ԓ�
                        from <= ct, ct < to }
        , -- �V�K�o�^����31���ȏ�90���ȓ��ɉ��債�����[�U�̐�
          count { <u> | -- �L��ad�ڐG�C�x���g
                        <ae, u, ad, _> <- +eua+
                      , -- ����ڐG���L��ad�ł���
                        is_first_contact u ae
                      , -- �V�K�o�^�R���o�[�W�����C�x���g
                        <_, u, touroku, ct1> <- +euc+
                      , -- �V�K�o�^�R���o�[�W�������w����ԓ�
                        from <= ct1, ct1 < to
                      , -- ����R���o�[�W�����C�x���g
                        <_, u, oubo, ct2> <- +euc+
                      , -- �V�K�o�^����31���ȏ�90���ȓ��ɉ���
                        ct1 + days 30 < ct2, ct2 <= ct1 + days 90
                      , -- �V�K�o�^����30���ȓ��ɂ͉��債�Ă��Ȃ�
                        not (exists { <u> | -- ����R���o�[�W�����C�x���g
                                            <_, u, oubo, ct3> <- +euc+
                                          , -- �V�K�o�^����30���ȓ��ɂ͉��債�Ă��Ȃ�
                                            ct1 < ct3, ct3 <= ct1 + days 30
                                          } ) }
        , -- �V�K�o�^����30���ȓ��ɉ��債�����[�U�̐�
          count { <u> | -- �L��ad�ڐG�C�x���g
                        <ae, u, ad, _> <- +eua+
                      , -- ����ڐG���L��ad�ł���
                        is_first_contact u ae
                      , -- �V�K�o�^�R���o�[�W�����C�x���g
                        <_, u, touroku, ct1> <- +euc+
                      , -- �V�K�o�^�R���o�[�W�������w����ԓ�
                        from <= ct1, ct1 < to
                      , -- ����R���o�[�W�����C�x���g
                        <_, u, oubo, ct2> <- +euc+
                      , -- �V�K�o�^����30���ȓ��ɉ���
                        ct1 < ct2, ct2 <= ct1 + days 30 }
        >
      | <ad,bg,cp,md,_,_,_> <- +ad+
      , <cp,cp_name> <- +cp+
      , <md,md_name> <- +md+ }")

(defparameter +business-goal-3-id+ 2)

(defparameter +business-goal-3-query+
  "-- �]�E�T�C�g �r�W�l�X�S�[���R�F����l��
   -- ����ڐG���ƂɈȉ����W�v
   -- �E�w����ԓ��ɐV�K�o�^�������[�U�̐�
   -- �E�V�K�o�^����30���ȓ��ɉ��債�����[�U�̐��i�⏕�w�W�j
   let bg := 2 in  -- ����l���r�W�l�X�S�[��
   let from := time ~A 00:00:00 in
   let to   := time ~A 00:00:00 in
   let oubo    := 0 in  -- ����R���o�[�W����
   let touroku := 1 in  -- �o�^�R���o�[�W����
   let is_first_contact u:int ae:int
               := -- ���[�Uu�ɂ��āA�L���ڐGae������ڐG���ǂ���
                  not (exists { <ae1> | <ae , u, _, at1> <- +eua+
                                      , <ae1, u, _, at2> <- +eua+
                                      , at2 < at1 })
   in { < cp_name, md_name
        , -- �w����ԓ��ɐV�K�o�^�������[�U�̐�
          count { <u> | -- �L��ad�ڐG�C�x���g
                        <ae, u, ad, _> <- +eua+
                      , -- ����ڐG���L��ad�ł���
                        is_first_contact u ae
                      , -- �V�K�o�^�R���o�[�W�����C�x���g
                        <_, u, touroku, ct> <- +euc+
                      , -- �V�K�o�^�R���o�[�W�������w����ԓ�
                        from <= ct, ct < to }
        , -- �V�K�o�^����30���ȓ��ɉ��債�����[�U�̐�
          count { <u> | -- �L��ad�ڐG�C�x���g
                        <ae, u, ad, _> <- +eua+
                      , -- ����ڐG���L��ad�ł���
                        is_first_contact u ae
                      , -- �V�K�o�^�R���o�[�W�����C�x���g
                        <_, u, touroku, ct1> <- +euc+
                      , -- �V�K�o�^�R���o�[�W�������w����ԓ�
                        from <= ct1, ct1 < to
                      , -- ����R���o�[�W�����C�x���g
                        <_, u, oubo, ct2> <- +euc+
                      , -- �V�K�o�^����30���ȓ��ɉ���
                        ct1 < ct2, ct2 <= ct1 + days 30 }
        >
      | <ad,bg,cp,md,_,_,_> <- +ad+
      , <cp,cp_name> <- +cp+
      , <md,md_name> <- +md+ }")


;;;
;;; Relations
;;;

;;; User: < User >
(defrelation +u+ (:int)
  (relation-adjoin-all (list (tuple 1)
                             (tuple 2))
                       (empty-relation)))

;;; Business Goal: < Business Goal, Name, Header, Query >
(defrelation +bg+ (:int :string :string :string)
  (relation-adjoin-all
    (list (tuple +business-goal-1-id+ "�Z������l��"
                 "�L�����y�[��,�}��,����o�^,��������,�i�R��������j"
                 +business-goal-1-query+)
          (tuple +business-goal-2-id+ "��������l��"
                 "�L�����y�[��,�}��,����o�^,�R��������,�i��������j"
                 +business-goal-2-query+)
          (tuple +business-goal-3-id+ "����l��"
                 "�L�����y�[��,�}��,����o�^,�i��������j"
                 +business-goal-3-query+))
    (empty-relation)))

(defun get-business-goal (name)
  (let ((query (format nil "let n := ~S
                            in { <i,n,h,q>
                               | <i,n,h,q> <- +bg+ }" name)))
    (first (relation->list (waql:eval-waql query)))))

(defun business-goal-id (business-goal)
  (tuple-ref business-goal 0))

(defun business-goal-name (business-goal)
  (tuple-ref business-goal 1))

(defun business-goal-header (business-goal)
  (tuple-ref business-goal 2))

(defun business-goal-dimension (business-goal)
  (let ((header (business-goal-header business-goal)))
    (length (split-sequence #\, header))))

(defun business-goal-query (business-goal)
  (tuple-ref business-goal 3))

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
(defrelation +ec+ (:int :int)
  (relation-adjoin-all (list (tuple 3 1)
                             (tuple 5 1))
                       (empty-relation)))

;;; Event Advertise: < Event, Advertise >
(defrelation +ea+ (:int :int)
  (relation-adjoin-all (list (tuple 1 1)
                             (tuple 2 2)
                             (tuple 4 2))
                       (empty-relation)))

;;; Conversion: < Conversion, Name >
(defrelation +cv+ (:int :string)
  (empty-relation))

;;; Advertise: < Advertise, Business Goal,
;;;              Campaign, Medium, Menu, Creative, Link >
(defrelation +ad+ (:int :int :int :int :int :int :int)
  (empty-relation))

;;; Campaign: < Campaign, Name >
(defrelation +cp+ (:int :string)
  (empty-relation))

;;; Medium: < Medium, Name >
(defrelation +md+ (:int :string)
  (empty-relation))

;;; Menu: < Menu, Name >
(defrelation +mn+ (:int :string)
  (empty-relation))

;;; Creative: < Creative, Name >
(defrelation +cr+ (:int :string)
  (empty-relation))

;;; Link: < Link, URL >
(defrelation +ln+ (:int :string)
  (empty-relation))

;;; Event-User-Advertise < Event, User, Advertise, Time >
(defrelation +eua+ (:int :int :int :time)
  (waql-in-sexp (query (ev u ad tm) (<- (ev u tm) +ev+)
                                    (<- (ev ad) +ea+))))

;;; Event-User-Conversion < Event, User, Conversion, Time >
(defrelation +euc+ (:int :int :int :time)
  (waql-in-sexp (query (ev u cv tm) (<- (ev u tm) +ev+)
                                    (<- (ev cv) +ec+))))

;;; < User, Advertise Event, Conversion Event >
(defrelation +uf1+ (:int :int :int)
  (waql-in-sexp (query (u ae ce) (<- (ae u _ at) +eua+)
                                 (<- (ce u _ ct) +euc+)
                                 (< at ct))))

(defun clear-events ()
  (setf +ev+ (empty-relation)))

(defun clear-event-advertises ()
  (setf +ea+ (empty-relation)))

(defun clear-event-conversions ()
  (setf +ec+ (empty-relation)))


;;;
;;; Querying
;;;

(defun execute-query-may (business-goal &key show-query-p)
  (execute-query business-goal "2013-5-1" "2013-6-1"
                 :show-query-p show-query-p))

(defun execute-query (business-goal from to &key show-query-p)
  (let ((bg (get-business-goal business-goal)))
    (let ((name (business-goal-name bg))
          (header (business-goal-header bg))
          (dimension (business-goal-dimension bg))
          (query (format nil (business-goal-query bg) from to)))
      ; show query if requested
      (when show-query-p
        (format t "~A~%" query))
      ; show business goal
      (format t "��~A~%" name)
      ; show header of the business goal
      (format t "~A~%" header)
      ; show results of the query
      (let ((results (relation->list (waql:eval-waql query))))
        (dolist (result results)
          (when (> (tuple-ref result 2) 0)
            (format t "~{~A~^,~}~%"
                    (loop for i from 0 below dimension
                       collect (tuple-ref result i)))))))))


;;;
;;; Read CV report
;;;
;;;   Read CV report represented in a S-expression of following protocol.
;;;
;;; Protocol:
;;;   ((<user-id> (:inflows ((<inflow-time> <media-name>)*)
;;;                :conversions ((<conversion-time> <conversion-name>)*)))*)
;;;
;;; Values:
;;;   <user-id> --- an integer
;;;   <inflow-time> --- a string for timestamp in ISO 8601 format, YYYY-MM-DDThh:mm:ss
;;;   <inflow-name> --- a string
;;;   <conversion-time> --- a string for timestamp in ISO 8601 format, YYYY-MM-DDThh:mm:ss
;;;   <conversion-name> --- a string
;;;

(defun read-cvreport (filespec)
  ;; first, clear relations
  (clear-events)
  (clear-event-advertises)
  (clear-event-conversions)
  ;; second, read cv report
  (with-open-file (in filespec :direction :input)
    (let ((users (read in)))
      (loop for user in users
            for i from 0
         do (when (= (mod i 100) 0)
              (format t "~A" "="))
            (let ((uid (user-id user))
                  (inflows (user-inflows user))
                  (conversions (user-conversions user)))
              (let ((u (get-user uid)))
                (dolist (inflow inflows)
                  (let ((time (inflow-time inflow))
                        (medium (inflow-medium inflow))
                        (campaign (inflow-campaign inflow)))
                    (when (type-campaign-p campaign)
                      (let* ((md (get-medium medium))
                             (cp (get-campaign campaign))
                             (ad (get-advertise cp md campaign))
                             (ev (add-event u time)))
                        (add-event-advertise ev ad)))))
                (dolist (conversion conversions)
                  (let ((time (conversion-time conversion))
                        (name (conversion-name conversion)))
                    (let ((cv (get-conversion name))
                          (ev (add-event u time)))
                      (add-event-conversion ev cv)))))))))
  ;; finally, compute +EUA+ and +EUC+ relations
  (setf +eua+ (waql:waql "{ <ev,u,ad,tm> | <ev,u,tm> <- +ev+
                                         , <ev,ad> <- +ea+ }")
        +euc+ (waql:waql "{ <ev,u,cv,tm> | <ev,u,tm> <- +ev+
                                         , <ev,cv> <- +ec+ }"))
  (values))


;;;
;;; Read CV report - selectors
;;;

(defun user-id (user)
  (car user))

(defun user-inflows (user)
  (getf (cadr user) :inflows))

(defun user-conversions (user)
  (getf (cadr user) :conversions))

(defun inflow-time (inflow)
  (parse-timestring (car inflow)))

(defun inflow-medium (inflow)
  (cadr inflow))

(defun inflow-campaign (inflow)
  (caddr inflow))

(defun conversion-time (conversion)
  (parse-timestring (car conversion)))

(defun conversion-name (conversion)
  (cadr conversion))


;;;
;;; Read CV report - actions
;;;

(defun get-user (uid)
  (relation-adjoin (tuple uid) +u+)
  uid)

(let ((id 0)
      (ht (make-hash-table :test #'equal)))
  (defun get-medium (name)
    (or (gethash name ht)
        (prog1 id
          (relation-adjoin (tuple id name) +md+)
          (setf (gethash name ht) id)
          (incf id)))))

(let ((id 0)
      (ht (make-hash-table :test #'equal)))
  (defun get-campaign (name)
    (or (gethash name ht)
        (prog1 id
          (relation-adjoin (tuple id name) +cp+)
          (setf (gethash name ht) id)
          (incf id)))))

(defun type-campaign-p (cp_name)
  (starts-with-subseq "type" cp_name))

(let ((id 0)
      (ht (make-hash-table :test #'equal)))
  (defun get-advertise (cp md cp_name)
    (let ((bg (business-goal-of-campaign cp_name)))
      (let ((key (cons bg md)))
        (or (gethash key ht)
            (prog1 id
              (relation-adjoin (tuple id bg cp md 0 0 0) +ad+)
              (setf (gethash key ht) id)
              (incf id)))))))


(defun business-goal-of-campaign (cp_name)
  (cond
    ((or (search "point" cp_name)
         (search "uu" cp_name))
     +business-goal-3-id+)
    ((search "kaiin" cp_name)
     +business-goal-2-id+)
    (t
     +business-goal-1-id+)))

(let ((id 0))
  (defun add-event (user time)
    (prog1 id
      (relation-adjoin (tuple id user time) +ev+)
      (incf id))))

(defun add-event-advertise (event advertise)
  (relation-adjoin (tuple event advertise) +ea+)
  (values))

(let ((id 0)
      (ht (make-hash-table :test #'equal)))
  (defun get-conversion (name)
    (or (gethash name ht)
        (prog1 id
          (relation-adjoin (tuple id name) +cv+)
          (setf (gethash name ht) id)
          (incf id)))))

(defun add-event-conversion (event conversion)
  (relation-adjoin (tuple event conversion) +ec+)
  (values))
