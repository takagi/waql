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
  "-- 転職サイト ビジネスゴール１：短期応募獲得
   -- 初回接触広告ごとに以下を集計
   -- ・指定期間内に新規登録したユーザの数
   -- ・新規登録から30日以内に応募したユーザの数
   -- ・新規登録から31日以上90日以内に応募したユーザの数（補助指標）
   let bg := 0 in  -- 短期応募獲得ビジネスゴール
   let from := time ~A 00:00:00 in
   let to   := time ~A 00:00:00 in
   let oubo    := 0 in  -- 応募コンバージョン
   let touroku := 1 in  -- 登録コンバージョン
   let is_first_contact u:int ae:int
               := -- ユーザuについて、広告接触aeが初回接触かどうか
                  not (exists { <ae1> | <ae , u, _, at1> <- +eua+
                                      , <ae1, u, _, at2> <- +eua+
                                      , at2 < at1 })
   in { < cp_name, md_name
        , -- 指定期間内に新規登録したユーザの数
          count { <u> | -- 広告ad接触イベント
                        <ae, u, ad, _> <- +eua+
                      , -- 初回接触が広告adである
                        is_first_contact u ae
                      , -- 新規登録コンバージョンイベント
                        <_, u, touroku, ct> <- +euc+
                      , -- 新規登録コンバージョンが指定期間内
                        from <= ct, ct < to }
        , -- 新規登録から30日以内に応募したユーザの数
          count { <u> | -- 広告ad接触イベント
                        <ae, u, ad, _> <- +eua+
                      , -- 初回接触が広告adである
                        is_first_contact u ae
                      , -- 新規登録コンバージョンイベント
                        <_, u, touroku, ct1> <- +euc+
                      , -- 新規登録コンバージョンが指定期間内
                        from <= ct1, ct1 < to
                      , -- 応募コンバージョンイベント
                        <_, u, oubo, ct2> <- +euc+
                      , -- 新規登録から30日以内に応募
                        ct1 < ct2, ct2 <= ct1 + days 30 }
        , -- 新規登録から31日以上90日以内に応募したユーザの数
          count { <u> | -- 広告ad接触イベント
                        <ae, u, ad, _> <- +eua+
                      , -- 初回接触が広告adである
                        is_first_contact u ae
                      , -- 新規登録コンバージョンイベント
                        <_, u, touroku, ct1> <- +euc+
                      , -- 新規登録コンバージョンが指定期間内
                        from <= ct1, ct1 < to
                      , -- 応募コンバージョンイベント
                        <_, u, oubo, ct2> <- +euc+
                      , -- 新規登録から31日以上90日以内に応募
                        ct1 + days 30 < ct2, ct2 <= ct1 + days 90
                      , -- 新規登録から30日以内には応募していない
                        not (exists { <u> | -- 応募コンバージョンイベント
                                            <_, u, oubo, ct3> <- +euc+
                                          , -- 新規登録から30日以内には応募していない
                                            ct1 < ct3, ct3 <= ct1 + days 30
                                          } ) }
        >
      | <ad,bg,cp,md,_,_,_> <- +ad+
      , <cp,cp_name> <- +cp+
      , <md,md_name> <- +md+ }")

(defparameter +business-goal-2-id+ 1)

(defparameter +business-goal-2-query+
  "-- 転職サイト ビジネスゴール２：長期応募獲得
   -- 初回接触広告ごとに以下を集計
   -- ・指定期間内に新規登録したユーザの数
   -- ・新規登録から31日以上90日以内に応募したユーザの数
   -- ・新規登録から30日以内に応募したユーザの数（補助指標）
   let bg := 1 in  -- 長期応募獲得ビジネスゴール
   let from := time ~A 00:00:00 in
   let to   := time ~A 00:00:00 in
   let oubo    := 0 in  -- 応募コンバージョン
   let touroku := 1 in  -- 登録コンバージョン
   let is_first_contact u:int ae:int
               := -- ユーザuについて、広告接触aeが初回接触かどうか
                  not (exists { <ae1> | <ae , u, _, at1> <- +eua+
                                      , <ae1, u, _, at2> <- +eua+
                                      , at2 < at1 })
   in { < cp_name, md_name
        , -- 指定期間内に新規登録したユーザの数
          count { <u> | -- 広告ad接触イベント
                        <ae, u, ad, _> <- +eua+
                      , -- 初回接触が広告adである
                        is_first_contact u ae
                      , -- 新規登録コンバージョンイベント
                        <_, u, touroku, ct> <- +euc+
                      , -- 新規登録コンバージョンが指定期間内
                        from <= ct, ct < to }
        , -- 新規登録から31日以上90日以内に応募したユーザの数
          count { <u> | -- 広告ad接触イベント
                        <ae, u, ad, _> <- +eua+
                      , -- 初回接触が広告adである
                        is_first_contact u ae
                      , -- 新規登録コンバージョンイベント
                        <_, u, touroku, ct1> <- +euc+
                      , -- 新規登録コンバージョンが指定期間内
                        from <= ct1, ct1 < to
                      , -- 応募コンバージョンイベント
                        <_, u, oubo, ct2> <- +euc+
                      , -- 新規登録から31日以上90日以内に応募
                        ct1 + days 30 < ct2, ct2 <= ct1 + days 90
                      , -- 新規登録から30日以内には応募していない
                        not (exists { <u> | -- 応募コンバージョンイベント
                                            <_, u, oubo, ct3> <- +euc+
                                          , -- 新規登録から30日以内には応募していない
                                            ct1 < ct3, ct3 <= ct1 + days 30
                                          } ) }
        , -- 新規登録から30日以内に応募したユーザの数
          count { <u> | -- 広告ad接触イベント
                        <ae, u, ad, _> <- +eua+
                      , -- 初回接触が広告adである
                        is_first_contact u ae
                      , -- 新規登録コンバージョンイベント
                        <_, u, touroku, ct1> <- +euc+
                      , -- 新規登録コンバージョンが指定期間内
                        from <= ct1, ct1 < to
                      , -- 応募コンバージョンイベント
                        <_, u, oubo, ct2> <- +euc+
                      , -- 新規登録から30日以内に応募
                        ct1 < ct2, ct2 <= ct1 + days 30 }
        >
      | <ad,bg,cp,md,_,_,_> <- +ad+
      , <cp,cp_name> <- +cp+
      , <md,md_name> <- +md+ }")

(defparameter +business-goal-3-id+ 2)

(defparameter +business-goal-3-query+
  "-- 転職サイト ビジネスゴール３：会員獲得
   -- 初回接触ごとに以下を集計
   -- ・指定期間内に新規登録したユーザの数
   -- ・新規登録から30日以内に応募したユーザの数（補助指標）
   let bg := 2 in  -- 会員獲得ビジネスゴール
   let from := time ~A 00:00:00 in
   let to   := time ~A 00:00:00 in
   let oubo    := 0 in  -- 応募コンバージョン
   let touroku := 1 in  -- 登録コンバージョン
   let is_first_contact u:int ae:int
               := -- ユーザuについて、広告接触aeが初回接触かどうか
                  not (exists { <ae1> | <ae , u, _, at1> <- +eua+
                                      , <ae1, u, _, at2> <- +eua+
                                      , at2 < at1 })
   in { < cp_name, md_name
        , -- 指定期間内に新規登録したユーザの数
          count { <u> | -- 広告ad接触イベント
                        <ae, u, ad, _> <- +eua+
                      , -- 初回接触が広告adである
                        is_first_contact u ae
                      , -- 新規登録コンバージョンイベント
                        <_, u, touroku, ct> <- +euc+
                      , -- 新規登録コンバージョンが指定期間内
                        from <= ct, ct < to }
        , -- 新規登録から30日以内に応募したユーザの数
          count { <u> | -- 広告ad接触イベント
                        <ae, u, ad, _> <- +eua+
                      , -- 初回接触が広告adである
                        is_first_contact u ae
                      , -- 新規登録コンバージョンイベント
                        <_, u, touroku, ct1> <- +euc+
                      , -- 新規登録コンバージョンが指定期間内
                        from <= ct1, ct1 < to
                      , -- 応募コンバージョンイベント
                        <_, u, oubo, ct2> <- +euc+
                      , -- 新規登録から30日以内に応募
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
    (list (tuple +business-goal-1-id+ "短期応募獲得"
                 "キャンペーン,媒体,会員登録,当月応募,（３ヶ月応募）"
                 +business-goal-1-query+)
          (tuple +business-goal-2-id+ "長期応募獲得"
                 "キャンペーン,媒体,会員登録,３ヶ月応募,（当月応募）"
                 +business-goal-2-query+)
          (tuple +business-goal-3-id+ "会員獲得"
                 "キャンペーン,媒体,会員登録,（当月応募）"
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
      (format t "▼~A~%" name)
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
